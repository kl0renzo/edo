open Notty
open Printf
open Notty_unix

type cursor_position = int * int
type modes = Insert | Normal | Command
type to_color = Normal | Bracet | Error | MatchedWord

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
  create_editor ()

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

(** displays the current status of the editor *)
let update_status editor =
  let x, y = editor.cursor in
  editor.status <-
    editor.filename ^ " | " ^ mode_to_string editor.mode ^ " | "
    ^ string_of_int x ^ ","
    ^ string_of_int (y + editor.scroll_position)

(** moves cursor to the left, if on the left side does nothing *)
let move_cursor_left editor : editor =
  let x, y = editor.cursor in
  match x with
  | 0 ->
      editor.cursor <- (x, y);
      editor
  | _ ->
      editor.cursor <- (x - 1, y);
      editor

(** moves cursor to the right *)
let move_cursor_right editor : editor =
  let x, y = editor.cursor in
  editor.cursor <- (x + 1, y);
  editor

(** moves cursor down *)
let move_cursor_down editor : editor =
  let x, y = editor.cursor in
  match y with
  | y when y = editor.visible_lines - 2 ->
      editor.scroll_position <- editor.scroll_position + 1;
      editor
  | _ ->
      editor.cursor <- (x, y + 1);
      editor

(** moves cursor up, if on top does nothing*)
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

(** updates the nth element of a list *)
let rec update_nth_element lst n new_value =
  match lst with
  | [] -> []
  | hd :: tl ->
      if n = 0 then new_value :: tl
      else hd :: update_nth_element tl (n - 1) new_value

(** gets the nth element of a list *)
let rec get_nth n lst =
  match lst with
  | [] -> failwith "Index out of bounds"
  | hd :: tl -> if n = 0 then hd else get_nth (n - 1) tl

(** removes the nth element from a list *)
let rec remove_nth_element n lst =
  match lst with
  | [] -> []
  | hd :: tl -> if n = 0 then tl else hd :: remove_nth_element (n - 1) tl

(** creates a sublist from a list between to indices *)
let rec sublist_from_to lst x y =
  match lst with
  | [] -> []
  | _ :: tl when x > 0 -> sublist_from_to tl (x - 1) (y - 1)
  | hd :: tl when y >= 0 -> hd :: sublist_from_to tl (x - 1) (y - 1)
  | _ -> []

(** moves cursor to the maximum right position *)
let move_cursor_to_max_right editor : editor =
  let _, y = Editor.get_pos editor in
  let line = get_nth y editor.content in
  editor.cursor <- (String.length line, y - editor.scroll_position);
  editor

(** moves cursor to the maximum left position *)
let move_cursor_to_max_left editor : editor =
  let _, y = Editor.get_pos editor in
  editor.cursor <- (0, y - editor.scroll_position);
  editor

(** combines two lines *)
let combine_lines x y editor =
  let lst = editor.content in
  let line_1 = get_nth x lst in
  let line_2 = get_nth y lst in
  editor.content <- update_nth_element lst x (line_1 ^ line_2);
  editor

(** inserts a char at the position of the cursor (+ scroll offset) *)
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

(** deletes a char at the position after the cursor (+ scroll offset) *)
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

(** deletes a char at the position before the cursor (+ scroll offset) *)
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

(** inserts an item at the nth position of a list *)
let insert_at_nth_position lst n item =
  let rec insert_helper acc remaining count =
    match (remaining, count) with
    | [], _ -> List.rev (item :: acc)
    | hd :: tl, 0 -> List.rev_append acc (item :: hd :: tl)
    | hd :: tl, c -> insert_helper (hd :: acc) tl (c - 1)
  in
  if n < 0 then invalid_arg "Negative index" else insert_helper [] lst n

(** inserts a line above the cursor *)
let insert_line_above editor =
  let content = editor.content in
  let _, y = Editor.get_pos editor in
  editor.content <- insert_at_nth_position content y "";
  let ed = ref editor in
  ed := move_cursor_to_max_left editor;
  ed := move_cursor_up editor;
  editor

(** inserts a line below the cursor *)
let insert_line_below editor =
  let content = editor.content in
  let _, y = Editor.get_pos editor in
  editor.content <- insert_at_nth_position content (y + 1) "";
  let ed = ref editor in
  ed := move_cursor_down editor;
  ed := move_cursor_to_max_left editor;
  editor

(** reads the content of a file and creates a list of lines *)
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

(** saves the file *)
let write_file editor =
  let filename = editor.filename in
  let content = editor.content in
  let output_channel = open_out filename in
  List.iter (fun line -> output_string output_channel (line ^ "\n")) content;
  close_out output_channel

(** sets the mode of the editor to insert - no commands,...*)
let insert_mode editor =
  let content = editor.content in
  let x, y = Editor.get_pos editor in
  let y = if y > List.length content - 1 then List.length content - 1 else y in
  let line = List.nth content y in
  let x = if x > String.length line then String.length line else x in
  editor.cursor <- (x, y - editor.scroll_position);
  editor.mode <- Insert;
  editor

(** gets the visible content of the editor - needed for scrolling *)
let get_visible_content editor : string list =
  let visible_lines = editor.visible_lines in
  let start_index = editor.scroll_position in
  let end_index =
    min (List.length editor.content) (editor.scroll_position + visible_lines)
  in
  sublist_from_to editor.content start_index end_index

(** split a string into words *)
let split_string_with_whitespace input_string =
  let rec split_acc input acc current_word =
    match input with
    | [] -> List.rev (List.append [ current_word ] acc)
    | first_char :: rest ->
        if Char.equal ' ' first_char then
          if current_word = "" then
            split_acc rest acc (String.make 1 first_char)
          else if String.ends_with ~suffix:" " current_word then
            split_acc rest acc (current_word ^ String.make 1 first_char)
          else split_acc rest (current_word :: acc) " "
        else if Char.equal '(' first_char || Char.equal ')' first_char then
          if current_word = "" then
            split_acc rest (String.make 1 first_char :: acc) ""
          else
            split_acc rest (String.make 1 first_char :: current_word :: acc) ""
        else if String.ends_with ~suffix:" " current_word then
          split_acc rest (current_word :: acc) (String.make 1 first_char)
        else split_acc rest acc (current_word ^ String.make 1 first_char)
  in
  split_acc (String.to_seq input_string |> List.of_seq) [] ""

let parse_line line = split_string_with_whitespace line

(** parse input into a list of list of words *)
let parse_input editor =
  let content = get_visible_content editor in
  let parsed_content = List.map parse_line content in
  parsed_content


let rec find_position_in_array line cur i pos =
  match line with
  | [] -> 0
  | x :: xs ->
      if cur + String.length x > pos then i
      else find_position_in_array xs (cur + String.length x) (i + 1) pos

(** defines the colors of the highlighted words, braces,... *)
let highlight_lines lines color_array =
  let map_line i line ii =
    (* the colors can be changed depending on the user theme *)
    match List.nth (List.nth color_array ii) i with
    | Normal -> I.(string A.(fg white) line)
    | Bracet -> I.(string A.(fg green ++ bg white) line)
    | Error -> I.(string A.(fg black ++ bg red) line)
    | MatchedWord -> I.(string A.(fg red ++ bg white) line)
  in

  List.mapi (fun j ll -> List.mapi (fun i line -> map_line i line j) ll) lines

(** find the matching closing brace *)
let find_matching_close_brace content x y =
  let r = List.rev content in
  let s = Stack.create () in
  let iter_lines xx v =
    if x + xx < List.length content then
      List.iteri
        (fun yy vv ->
          if x + 1 + xx < List.length content || y + 1 + yy < List.length v then
            match vv with
            | "(" ->
                let _ = Stack.pop_opt s in
                ()
            | ")" ->
                Stack.push
                  (List.length content - xx - 1, List.length v - yy - 1)
                  s
            | _ -> ())
        (List.rev v)
  in
  List.iteri iter_lines r;
  Stack.pop_opt s

(** find the matching opening brace *)
let find_matching_open_brace content x y =
  let s = Stack.create () in
  let iter_lines xx v =
    if xx <= x then
      List.iteri
        (fun yy vv ->
          if xx < x || yy < y then
            match vv with
            | ")" ->
                let _ = Stack.pop_opt s in
                ()
            | "(" -> Stack.push (xx, yy) s
            | _ -> ())
        v
  in
  List.iteri iter_lines content;
  Stack.pop_opt s

(** find unmatched braces *)
let find_unmatched_braces content =
  let stack = Stack.create () in
  let unmatched_braces = ref [] in

  let check_brace x y c =
    match c with
    | "(" -> Stack.push (x, y) stack
    | ")" ->
        if Stack.is_empty stack then
          unmatched_braces := (x, y) :: !unmatched_braces
        else
          let _ = Stack.pop stack in
          ()
    | _ -> ()
  in

  List.iteri
    (fun x line ->
      List.iteri (fun y c -> check_brace x y c) line)
    content;

  (* Add any remaining unmatched opening parentheses to the list *)
  while not (Stack.is_empty stack) do
    let x, y = Stack.pop stack in
    unmatched_braces := (x, y) :: !unmatched_braces
  done;

  !unmatched_braces

(** mark matching (equal) words *)
let mark_matched_words parsed_lines codes x pos_word =
  let word = List.nth (List.nth parsed_lines x) pos_word in
  let check_word x y =
    if List.nth (List.nth parsed_lines x) y = word then true else false
  in
  List.mapi
    (fun i line ->
      List.mapi
        (fun ii word -> if check_word i ii then MatchedWord else word)
        line)
    codes

(** Mark unmatched opening parentheses with a specific color *)
let mark_unmatched_braces color_codes brace_positions =
    List.mapi
      (fun i line ->
        List.mapi
          (fun ii word ->
            if List.mem (i, ii) brace_positions then Error else word)
          line)
      color_codes

(** highlight the code - some depend on the cursor position *)
let highlight parsed_lines cursor unmatched_braces =
  let codes = List.map (List.map (fun _ -> Normal)) parsed_lines in
  let line_as_str i = String.concat "" (List.nth parsed_lines i) in

  let codes = mark_unmatched_braces codes unmatched_braces in

  match cursor with
  | y, x when x < List.length parsed_lines && y < String.length (line_as_str x)
    ->
      let current_char = String.get (line_as_str x) y in
      (* find the closing bracket *)
      if current_char = '(' then
        let pos = find_position_in_array (List.nth parsed_lines x) 0 0 y in
        let match_brace = find_matching_close_brace parsed_lines x pos in
        match match_brace with
        | Some (a, b) ->
            List.mapi
              (fun i x ->
                if i = a then
                  (List.mapi (fun ii xx -> if ii = b then Bracet else xx)) x
                else x)
              codes
        | None -> codes
      else if current_char = ')' then
        let pos = find_position_in_array (List.nth parsed_lines x) 0 0 y in
        let match_brace = find_matching_open_brace parsed_lines x pos in
        match match_brace with
        | Some (a, b) ->
            List.mapi
              (fun i x ->
                if i = a then
                  (List.mapi (fun ii xx -> if ii = b then Bracet else xx)) x
                else x)
              codes
        | None -> codes
      else if current_char != ' ' then
        let pos = find_position_in_array (List.nth parsed_lines x) 0 0 y in
        let codes_with_matched_words =
          mark_matched_words parsed_lines codes x pos
        in
        codes_with_matched_words
      else codes
  | _, _ -> codes

let rec main_loop editor t =
  let ed = ref editor in
  update_status editor;
  let status_image = I.string A.(fg blue) editor.status in
  let parsed_lines = parse_input editor in
  let unmatched_braces = find_unmatched_braces parsed_lines in
  let highlight_code = highlight parsed_lines editor.cursor unmatched_braces in
  let parsed_highlighted_lines = highlight_lines parsed_lines highlight_code in
  let text_images = List.map I.hcat parsed_highlighted_lines in
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
      | `Key (`ASCII 'q', _) -> ()
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
      let t = Term.create () in
      main_loop editor t)
