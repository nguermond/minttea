module History = struct
  (* History is represented as a pair of stacks.
   * This allows insertion at a certain point in history.
   * For example, say we are given history
   *    "hello", "wold", "asdf", "qwer", [""]
   * we can edit "wold" to "world" to obtain the history
   * "hello", "wold", ["world"], "asdf", "qwer"
   *        ^             ^             ^
   *    bwd stack       buffer       fwd stack
   * Once "world" is pushed in the history we obtain the history
   * "hello", "wold", "world", [""], "asdf", "qwer"
   *)
     
  type t = { active : bool;     (* whether history is active *)
             fwd : string list;
             bwd : string list;
             buffer : string option;
           }
         
  let empty : t =
    { active = false; buffer = None; fwd = []; bwd = [] }
    
  let push_fwd str h : t =
    { h with fwd = str :: h.fwd }
    
  let push_bwd str h : t =
    { h with bwd = str :: h.bwd }

  let pop_fwd h : (string option) * t =
    match h.fwd with
    | [] -> (None, h)
    | x :: xs -> (Some x, {h with fwd = xs })

  let pop_bwd h : (string option) * t =
    match h.bwd with
    | [] -> (None, h)
    | x :: xs -> (Some x, {h with bwd = xs })

  let bwd h =
    let (h, buffer) =
      match pop_bwd h, h.buffer with
      | (None,h), _ -> (h, h.buffer)
      | (Some value,h), None -> (h, Some value)
      | (Some value,h), Some buf -> (push_fwd buf h, Some value)
    in {h with buffer}
     
  let fwd h =
    let (h, buffer) =
      match pop_fwd h, h.buffer with
      | (None, h), None -> (h,None)
      | (None, h), Some buf -> (push_bwd buf h, None)
      | (Some value, h), None -> (h, Some value)
      | (Some value, h), Some buf -> (push_bwd buf h, Some value)
    in {h with buffer }
     
  let push str h =
    let h = (match h.buffer with
    | None -> h
    | Some v -> push_bwd v h) in
    let h = push_bwd str h in
    { h with buffer = None; active = true }
end


type t = {
  (* text field *)
  value : string;
  placeholder : string;
  prompt : string;
  last_action : float;
  (* cursor *)
  cursor : Cursor.t;
  pos : int;
  (* styles *)
  text_style : Spices.style;
  placeholder_style : Spices.style;
  (* history *)
  history : History.t;
}

(* Utils *)
open struct
  let cursor_at_beginning t = t.pos = 0
  let cursor_at_end t = t.pos = String.length t.value
  let now_secs () = Ptime_clock.now () |> Ptime.to_float_s

  (** Cursor will be at the start of the right part. *)
  let split s ~at =
    let left = String.sub s 0 at in
    let right = String.sub s at (String.length s - at) in
    (left, right)
end

let default_prompt = "> "
let default_placeholder = ""
let default_text_style = Spices.default
let default_placeholder_style = Spices.default |> Spices.faint true
let default_cursor () = Cursor.make ()
let resume_blink_after = 0.25

let make value
    ?(text_style = default_text_style)
    ?(placeholder_style = default_placeholder_style)
    ?(cursor = default_cursor ())
    ?(placeholder = default_placeholder)
    ?(prompt = default_prompt) () =
  let history = History.empty in
  let value, pos =
    if String.length value = 0 then ("", 0) else (value, String.length value)
  in
  {
    value;
    placeholder;
    pos;
    text_style;
    placeholder_style;
    cursor;
    prompt;
    last_action = now_secs ();
    history;
  }

let empty () = make "" ()

let placeholder_view t =
  let placeholder_style = Spices.(t.placeholder_style |> build) in
  let text_style = Spices.(t.text_style |> build) in

  let result = ref "" in

  for i = 0 to String.length t.placeholder - 1 do
    let s = String.make 1 @@ t.placeholder.[i] in
    let txt =
      if i = 0 then Cursor.view t.cursor ~text_style:t.placeholder_style s
      else placeholder_style "%s" s
    in
    result := !result ^ txt
  done;

  text_style "%s" t.prompt ^ !result

let view t =
  if String.length t.value = 0 then placeholder_view t
  else
    let text_style = Spices.(t.text_style |> build) in

    let result = ref "" in
    for i = 0 to String.length t.value - 1 do
      let s = String.make 1 @@ t.value.[i] in
      let txt =
        if i = t.pos then Cursor.view t.cursor ~text_style:t.text_style s
        else text_style "%s" s
      in
      result := !result ^ txt
    done;

    if cursor_at_end t then
      result := !result ^ Cursor.view t.cursor ~text_style:t.text_style " ";

    text_style "%s" t.prompt ^ !result

let current_text t = t.value

let write t s =
  match t with
  | t when cursor_at_beginning t ->
      { t with value = s ^ t.value; pos = t.pos + String.length s }
  | t when cursor_at_end t ->
      { t with value = t.value ^ s; pos = t.pos + String.length s }
  | _ ->
      let left, right = split t.value ~at:t.pos in
      let new_value = left ^ s ^ right in
      { t with value = new_value; pos = t.pos + String.length s }

let space t = write t " "

let backspace t =
  if t.pos = 0 then t
  else
    let left, right = split t.value ~at:t.pos in
    (* drop the last char from the left part *)
    let left = String.sub left 0 (t.pos - 1) in
    let new_value = left ^ right in
    { t with value = new_value; pos = t.pos - 1 }

let move_cursor t action =
  let pos =
    match action with
    | `Character_backward when t.pos > 0 -> t.pos - 1
    | `Character_forward when t.pos < String.length t.value -> t.pos + 1
    | `Jump_to_beginning -> 0
    | `Jump_to_end -> String.length t.value
    | _ -> t.pos
  in
  { t with pos }

let character_backward t = move_cursor t `Character_backward
let character_forward t = move_cursor t `Character_forward
let jump_to_beginning t = move_cursor t `Jump_to_beginning [@@warning "-32"]
let jump_to_end t = move_cursor t `Jump_to_end

let history_bwd t =
  if t.history.active then
    let h = t.history in
    let h = History.bwd h in
    match h.buffer with
    | None -> { t with history = h }
    | Some value -> { t with value; history = h } |> jump_to_end
  else t
  
let history_fwd t =
  if t.history.active then
    let h = t.history in
    let h = History.fwd h in
    match h.buffer with 
    | None -> { t with value = ""; history = h } |> jump_to_end
    | Some value -> { t with value; history = h } |> jump_to_end
  else t
  

let update t (e : Minttea.Event.t) =
  match e with
  | KeyDown (key, modifier) ->
      let s =
        match (key, modifier) with
        (* Movement *)
        | Key s, Ctrl when s = "a" -> jump_to_beginning t
        | Key s, Ctrl when s = "e" -> jump_to_end t

        | Left, _ -> character_backward t
        | Key s, Ctrl when s = "b" -> character_backward t

        | Right, _ -> character_forward t
        | Key s, Ctrl when s = "f" -> character_forward t

        (* History *)
        | Down, _ -> history_fwd t
        | Up, _ -> history_bwd t

        (* Typing *)
        | Backspace, _ -> backspace t
        | Key s, _ -> write t s
        | Space, _ -> space t
        | Escape, _ | Enter, _ -> t
      in

      { s with cursor = Cursor.focus t.cursor; last_action = now_secs () }
  | _ ->
      let time_since_last_action = now_secs () -. t.last_action in

      let updated_cursor =
        if time_since_last_action <= resume_blink_after then
          Cursor.enable_blink t.cursor
        else t.cursor
      in
      { t with cursor = Cursor.update updated_cursor e }

let set_text value t = { t with value } |> jump_to_end

let focus b t = { t with cursor = if b then Cursor.focus t.cursor
                                  else Cursor.unfocus t.cursor }

let push_to_history str t =
  let history = History.push str t.history in
  { t with history }

let set_prompt str t =
  let prompt = str in
  { t with prompt }
