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
  mutable status : string;
  mutable scroll_position : int;
  mutable visible_lines : int;
}

let create_editor () =
  {
    content = [];
    cursor = (0, 0);
    mode = Normal;
    filename = "";
    status = "";
    scroll_position = 0;
    visible_lines = 0;
  }

module Editor = struct
  let create_editor = create_editor ()

  (*this is the real position in the file, editor.cursor is the position in the visible field
    the real position is needed for file editing
  *)
  let get_pos editor =
    let x, y = editor.cursor in
    (x, y + editor.scroll_position)
end

let mode_to_string mode =
  match mode with
  | Insert -> "Insert Mode"
  | Normal -> "Normal Mode"
  | Command -> "Command Mode"

let update_status editor =
  let x, y = editor.cursor in
  editor.status <-
    mode_to_string editor.mode ^ " | " ^ string_of_int x ^ "," ^ string_of_int y

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
  match y with
  | y when y = editor.visible_lines - 2 ->
      (*TODO why -2*)
      editor.scroll_position <- editor.scroll_position + 1;
      editor
  | _ ->
      editor.cursor <- (x, y + 1);
      editor

let move_cursor_up editor : editor =
  let x, y = editor.cursor in
  match y with
  | y when y = 0 && editor.scroll_position != 0 ->
      editor.scroll_position <- editor.scroll_position - 1;
      editor
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

let rec sublist_from_to lst x y =
  match lst with
  | [] -> []
  | _ :: tl when x > 0 -> sublist_from_to tl (x - 1) (y - 1)
  | hd :: tl when y >= 0 -> hd :: sublist_from_to tl (x - 1) (y - 1)
  | _ -> []

let move_cursor_to_max_right editor : editor =
  let _, y = Editor.get_pos editor in
  let line = get_nth y editor.content in
  editor.cursor <- (String.length line, y - editor.scroll_position);
  editor

let move_cursor_to_max_left editor : editor =
  let _, y = Editor.get_pos editor in
  editor.cursor <- (0, y - editor.scroll_position);
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
  let x, y = Editor.get_pos editor in
  let line = List.nth content y in
  let prefix = String.sub line 0 x in
  let suffix = String.sub line x (String.length line - x) in
  let new_line = String.concat "" [ prefix; String.make 1 char; suffix ] in
  editor.content <- update_nth_element content y new_line;
  let ed = ref editor in
  ed := move_cursor_right !ed;
  editor

let delete_char_after editor =
  let content = editor.content in
  let x, y = Editor.get_pos editor in
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
  let x, y = Editor.get_pos editor in
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

let insert_line_above editor =
  let content = editor.content in
  let _, y = Editor.get_pos editor in
  editor.content <- insert_at_nth_position content y "";
  let ed = ref editor in
  ed := move_cursor_to_max_left editor;
  ed := move_cursor_up editor;
  editor

let insert_line_below editor =
  let content = editor.content in
  let _, y = Editor.get_pos editor in
  editor.content <- insert_at_nth_position content (y + 1) "";
  let ed = ref editor in
  ed := move_cursor_down editor;
  ed := move_cursor_to_max_left editor;
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
  let x, y = Editor.get_pos editor in
  let y = if y > List.length content - 1 then List.length content - 1 else y in
  let line = List.nth content y in
  let x = if x > String.length line then String.length line else x in
  editor.cursor <- (x, y - editor.scroll_position);
  editor.mode <- Insert;
  editor

let get_visible_content editor : string list =
  let visible_lines = editor.visible_lines in
  let start_index = editor.scroll_position in
  let end_index =
    min (List.length editor.content) (editor.scroll_position + visible_lines)
  in
  sublist_from_to editor.content start_index end_index

let rec main_loop editor t =
  let ed = ref editor in
  update_status editor;
  let status_image = I.string A.(fg black) editor.status in
  let content = get_visible_content editor in
  let text_images = List.map (fun line -> I.string A.(fg white) line) content in
  let combined_text_image = I.vcat text_images in
  let combined_image = I.(status_image <-> hpad 1 0 combined_text_image) in
  let cursor_pos = (fst editor.cursor + 1, snd editor.cursor + 1) in
  Term.image t combined_image;
  Term.cursor t (Some cursor_pos);
  match editor.mode with
  | Normal -> (
      match Term.event t with
      | `Key (`ASCII 'h', _) | `Key (`Arrow `Left, _) ->
          ed := move_cursor_left editor;
          main_loop !ed t
      | `Key (`ASCII 'l', _) | `Key (`Arrow `Right, _) ->
          ed := move_cursor_right editor;
          main_loop !ed t
      | `Key (`ASCII 'k', _) | `Key (`Arrow `Up, _) ->
          ed := move_cursor_up editor;
          main_loop !ed t
      | `Key (`ASCII 'j', _) | `Key (`Arrow `Down, _) ->
          ed := move_cursor_down editor;
          main_loop !ed t
      | `Key (`ASCII 'i', _) ->
          ed := insert_mode editor;
          main_loop !ed t
      | `Key (`ASCII ':', _) ->
          !ed.mode <- Command;
          main_loop !ed t
      | `Key (`ASCII 'o', _) ->
          ed := insert_line_below editor;
          main_loop editor t
      | `Key (`ASCII 'O', _) ->
          ed := insert_line_above editor;
          main_loop editor t
      | `Key (`ASCII 'z', _) ->
          write_file editor;
          main_loop editor t (*todo save file, and maybe change keybinding*)
      | `Key (`ASCII 'x', _) | `Key (`Delete, _) ->
          ed := delete_char_after editor;
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
          ed := insert_line_below editor;
          main_loop editor t
      | `Key (`Arrow `Left, _) ->
          ed := move_cursor_left editor;
          main_loop !ed t
      | `Key (`Arrow `Right, _) ->
          ed := move_cursor_right editor;
          main_loop !ed t
      | `Key (`Arrow `Up, _) ->
          ed := move_cursor_up editor;
          main_loop !ed t
      | `Key (`Arrow `Down, _) ->
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
    let _, y = ANSITerminal.size () in
    editor.visible_lines <- y;
    editor.status <- "Normal Mode";
    editor.mode <- Normal;
    update_status editor;

    if Sys.file_exists filename then (
      let file_content = read_file filename in
      editor.content <- file_content;
      let t = Term.create () in
      main_loop editor t)
    else (
      editor.content <- [ "" ];
      (*TODO: fix issue when iserting and this is just []*)
      let t = Term.create () in
      main_loop editor t)
