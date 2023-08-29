open Notty
open Printf
open Notty_unix

type cursor_position = int * int
type modes = Insert | Normal | Command

type editor = {
  mutable content : string list;
  mutable cursor : cursor_position;
  mutable mode : modes;
  mutable filename : string;
}

let create_editor () =
  { content = []; cursor = (0, 0); mode = Normal; filename = "" }

(*
TODO: limitiations on max cursor positions
*)
let move_cursor_left editor : editor =
  let x, y = editor.cursor in
  match x with
  | 0 ->
      editor.cursor <- (x, y);
      editor
  | _ ->
      editor.cursor <- (x - 1, y);
      editor

let move_cursor_right editor : editor =
  let x, y = editor.cursor in
  editor.cursor <- (x + 1, y);
  editor

let move_cursor_down editor : editor =
  let x, y = editor.cursor in
  editor.cursor <- (x, y + 1);
  editor

let move_cursor_up editor : editor =
  let x, y = editor.cursor in
  match y with
  | 0 ->
      editor.cursor <- (x, y);
      editor
  | _ ->
      editor.cursor <- (x, y - 1);
      editor

let rec update_nth_element lst n new_value =
  match lst with
  | [] -> []
  | hd :: tl ->
      if n = 0 then new_value :: tl
      else hd :: update_nth_element tl (n - 1) new_value

let rec get_nth n lst =
  match lst with
  | [] -> failwith "Index out of bounds"
  | hd :: tl -> if n = 0 then hd else get_nth (n - 1) tl

let move_cursor_to_max_right editor : editor =
  let _, y = editor.cursor in
  let line = get_nth y editor.content in
  editor.cursor <- (String.length line, y);
  editor

let rec remove_nth_element n lst =
  match lst with
  | [] -> []
  | hd :: tl -> if n = 0 then tl else hd :: remove_nth_element (n - 1) tl

let combine_lines x y editor =
  let lst = editor.content in
  let line_1 = get_nth x lst in
  let line_2 = get_nth y lst in
  editor.content <- update_nth_element lst x (line_1 ^ line_2);
  editor

let insert_char char editor =
  let content = editor.content in
  let x, y = editor.cursor in
  let line = List.nth content y in
  let prefix = String.sub line 0 x in
  let suffix = String.sub line x (String.length line - x) in
  let new_line = String.concat "" [ prefix; String.make 1 char; suffix ] in
  editor.content <- update_nth_element content y new_line;
  let ed = ref editor in
  ed := move_cursor_right !ed;
  editor

let delete_char editor =
  let content = editor.content in
  let x, y = editor.cursor in
  if y >= List.length content then editor
  else
    let line = List.nth content y in
    if x >= String.length line then editor
    else
      let prefix = String.sub line 0 x in
      let suffix = String.sub line (x + 1) (String.length line - x - 1) in
      let new_line = String.concat "" [ prefix; suffix ] in
      editor.content <- update_nth_element content y new_line;
      editor

let delete_char_before editor =
  let ed = ref editor in
  let content = editor.content in
  let x, y = editor.cursor in
  match (x, y) with
  | x, y when x = 0 && y = 0 -> editor
  | _, y when y > List.length content -> editor
  | _ -> (
      let line = List.nth content y in
      match x with
      | x when x > String.length line -> editor
      | x when x = 0 ->
          ed := combine_lines (y - 1) y editor;
          editor.content <- remove_nth_element y editor.content;
          ed := move_cursor_up !ed;
          ed := move_cursor_to_max_right !ed;
          editor
      | _ ->
          let prefix = String.sub line 0 (x - 1) in
          let suffix = String.sub line x (String.length line - x) in
          let new_line = String.concat "" [ prefix; suffix ] in
          editor.content <- update_nth_element content y new_line;
          let ed = ref editor in
          ed := move_cursor_left !ed;
          editor)

let insert_at_nth_position lst n item =
  let rec insert_helper acc remaining count =
    match (remaining, count) with
    | [], _ -> List.rev (item :: acc)
    | hd :: tl, 0 -> List.rev_append acc (item :: hd :: tl)
    | hd :: tl, c -> insert_helper (hd :: acc) tl (c - 1)
  in
  if n < 0 then invalid_arg "Negative index" else insert_helper [] lst n

let insert_new_line_above editor =
  let content = editor.content in
  let _, y = editor.cursor in
  editor.content <- insert_at_nth_position content y "";
  editor.cursor <- (0, y);
  editor

let insert_new_line_below editor =
  let content = editor.content in
  let _, y = editor.cursor in
  editor.content <- insert_at_nth_position content (y + 1) "";
  editor.cursor <- (0, y + 1);
  editor

let read_file filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true do
      lines := input_line chan :: !lines
    done;
    !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let write_file editor =
  let filename = editor.filename in
  let content = editor.content in
  let output_channel = open_out filename in
  List.iter (fun line -> output_string output_channel (line ^ "\n")) content;
  close_out output_channel

let insert_mode editor =
  let content = editor.content in
  let x, y = editor.cursor in
  let y = if y > List.length content - 1 then List.length content - 1 else y in
  let line = List.nth content y in
  let x = if x > String.length line then String.length line else x in
  editor.cursor <- (x, y);
  editor.mode <- Insert;
  editor

let rec main_loop editor t =
  let ed = ref editor in
  let text_images =
    List.map (fun line -> I.string A.(fg white) line) editor.content
  in
  let combined_image = I.vcat text_images in
  Term.image t combined_image;
  Term.cursor t (Some editor.cursor);
  match editor.mode with
  | Normal -> (
      match Term.event t with
      | `Key (`ASCII 'h', _) | `Key (`Arrow `Left,_) ->
          ed := move_cursor_left editor;
          main_loop !ed t
      | `Key (`ASCII 'l', _) | `Key (`Arrow `Right,_) ->
          ed := move_cursor_right editor;
          main_loop !ed t
      | `Key (`ASCII 'k', _) | `Key (`Arrow `Up,_) ->
          ed := move_cursor_up editor;
          main_loop !ed t
      | `Key (`ASCII 'j', _) | `Key (`Arrow `Down,_) ->
          ed := move_cursor_down editor;
          main_loop !ed t
      | `Key (`ASCII 'i', _) ->
          ed := insert_mode editor;
          main_loop !ed t
      | `Key (`ASCII ':', _) ->
          !ed.mode <- Command;
          main_loop !ed t
      | `Key (`ASCII 'o', _) ->
          ed := insert_new_line_above editor;
          main_loop editor t
      | `Key (`ASCII 'O', _) ->
          ed := insert_new_line_below editor;
          main_loop editor t
      | `Key (`ASCII 'z', _) ->
          write_file editor;
          main_loop editor t (*todo save file, and maybe change keybinding*)
      | `Key (`ASCII 'x', _) ->
          ed := delete_char editor;
          main_loop !ed t
      | `Key (`ASCII 'Z', _) -> ()
      | _ -> main_loop editor t (*TODO: this should be delete char*))
  | Insert -> (
      match Term.event t with
      | `End | `Key (`Escape, []) ->
          editor.mode <- Normal;
          main_loop editor t
      | `Key (`ASCII c, _) ->
          ed := insert_char c editor;
          main_loop editor t
      | `Key (`Enter, _) ->
          ed := insert_new_line_below editor;
          main_loop editor t
      | `Key (`Arrow `Left,_) ->
        ed := move_cursor_left editor;
        main_loop !ed t
      | `Key (`Arrow `Right,_) ->
          ed := move_cursor_right editor;
          main_loop !ed t
      | `Key (`Arrow `Up,_) ->
          ed := move_cursor_up editor;
          main_loop !ed t
      | `Key (`Arrow `Down,_) ->
          ed := move_cursor_down editor;
          main_loop !ed t
      | `Key (`Backspace, _) ->
          ed := delete_char_before editor;
          main_loop editor t
      | _ -> main_loop editor t)
  | Command -> (
      match Term.event t with
      | `End | `Key (`Escape, []) ->
          editor.mode <- Normal;
          main_loop editor t
      | _ -> main_loop editor t)

let () =
  if Array.length Sys.argv <> 2 then
    printf "Usage: %s <filename>\n" Sys.argv.(0)
  else
    let editor = create_editor () in
    let filename = Sys.argv.(1) in
    editor.filename <- filename;

    if Sys.file_exists filename then (
      let file_content = read_file filename in
      editor.content <- file_content;
      let t = Term.create () in
      main_loop editor t)
    else editor.content <- [ "" ];
    (*TODO: fix issue when iserting and this is just []*)
    let t = Term.create () in
    main_loop editor t
