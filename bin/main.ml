open Notty
open Notty_unix

type cursor_position = int * int

type modes =
  | Insert
  | Normal
  | Command

type editor = {
  mutable content: string list;
  mutable cursor: cursor_position;
  mutable mode: modes;
}

let create_editor () = {
  content = [];
  cursor = (0,0);
  mode = Normal;
}

(*
TODO: limitiations on max cursor positions
*)
let move_cursor_left editor : editor =
  let x,y = editor.cursor in
  match x with 
  | 0 -> editor.cursor <- (x,y); editor;
  | _ -> editor.cursor <- (x-1, y); editor;;

let move_cursor_right editor : editor =
  let x,y = editor.cursor in
  editor.cursor <- (x+1, y); editor;;

let move_cursor_up editor : editor =
  let x,y = editor.cursor in
  editor.cursor <- (x, y+1); editor;;

let move_cursor_down editor : editor =
  let x,y = editor.cursor in
  match x with 
  | 0 -> editor.cursor <- (x,y); editor;
  | _ -> editor.cursor <- (x, y-1); editor;;

  let rec update_nth_element lst n new_value =
  match lst with
  | [] -> []
  | hd :: tl ->
      if n = 0 then
        new_value :: tl
      else
        hd :: update_nth_element tl (n - 1) new_value

let insert_char char editor =
  let content = editor.content in
  let position = editor.cursor in
  let x,y = position in
  let line = List.nth content y in
  let prefix = String.sub line 0 x in
  let suffix = String.sub line x (String.length line - x) in
  let new_line = String.concat "" [prefix; (String.make 1 char); suffix] in
  editor.content <- update_nth_element content y new_line;
  let ed = ref editor in
  ed := move_cursor_right !ed;
  editor;;

let rec main_loop editor t =
    let ed = ref editor in
    let text_images = List.map (fun line -> I.string A.(fg white) line) editor.content in
    let combined_image = I.vcat text_images in
    Term.image t combined_image;
    Term.cursor t (Some editor.cursor);
    match editor.mode with
    | Normal -> (
      match Term.event t with
      | `Key (`ASCII 'h',_) -> ed := move_cursor_left editor; main_loop !ed t;
      | `Key (`ASCII 'l',_) -> ed := move_cursor_right editor; main_loop !ed t;
      | `Key (`ASCII 'k',_) -> ed := move_cursor_down editor; main_loop !ed t;
      | `Key (`ASCII 'j',_) -> ed := move_cursor_up editor; main_loop !ed t;
      | `Key (`ASCII 'i',_) -> !ed.mode <- Insert; main_loop !ed t;
      | `Key (`ASCII ':',_) -> !ed.mode <- Command; main_loop !ed t;
      | `End | `Key (`Escape, [])  -> ()
      | _ -> main_loop editor  t
    )
    | Insert ->(
      match Term.event t with
      | `End | `Key (`Escape, [])  -> editor.mode <- Normal; main_loop editor t;
      | `Key (`ASCII c, _)      -> ed := insert_char c editor; main_loop editor t;
      | _ -> main_loop editor  t
    )
    | Command -> (
      match Term.event t with
      | `End | `Key (`Escape, [])  -> editor.mode <- Normal; main_loop editor t;
      | `Key (`ASCII 'z',_) -> main_loop editor t; (*todo save file, and maybe change keybinding*)
      | _ -> main_loop editor  t
    )

let () =
  let editor = create_editor () in
  let text_lines = [
    "Hello,";
    "This is a multi-line text example";
    "using notty and OCaml!";
  ] in
  editor.content <- text_lines;
  let t = Term.create () in main_loop editor t

