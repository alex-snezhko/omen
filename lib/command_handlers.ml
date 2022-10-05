open Notty
open Notty_unix
open Term_render
open Types
open Utils

let copy_file input_name output_name =
  let sanitize_spaces path =
    let regex = Str.regexp_string " " in
    Str.global_replace regex "\\ " path
  in
  ignore @@ Sys.command
  @@ Printf.sprintf "cp -r %s %s"
       (sanitize_spaces input_name)
       (sanitize_spaces output_name)

let rec multiple_choice ctx prompt above_prompt_img actions =
  render ctx (Some prompt) above_prompt_img;
  match Term.event ctx.term with
  | `Key (`Escape, _) -> ctx.state
  | `Key (`ASCII key, _) -> (
      let action = List.find_opt (fun (k, _) -> k = key) actions in
      match action with
      | None -> multiple_choice ctx prompt above_prompt_img actions
      | Some (_, fn) -> fn ())
  | _ -> multiple_choice ctx prompt above_prompt_img actions

let gather_input ctx prompt_prefix above_prompt_img initial_input on_complete =
  let rec recurse typed_input =
    let prompt = Printf.sprintf "%s %s|" prompt_prefix typed_input in
    render ctx (Some prompt) above_prompt_img;
    match Term.event ctx.term with
    | `Key (`Escape, _) -> ()
    | `Key (`Enter, _) -> on_complete typed_input
    | `Key (`ASCII c, _) -> recurse (typed_input ^ String.make 1 c)
    | `Key (`Backspace, _) ->
        let str =
          String.sub typed_input 0 (max 0 (String.length typed_input - 1))
        in
        recurse str
    | _ -> recurse typed_input
  in
  recurse initial_input

let notification_prompt ctx prompt above_prompt_img =
  render ctx (Some (prompt ^ "Press any key to continue.")) above_prompt_img;
  ignore (Term.event ctx.term)

let rec rec_rm path =
  if Sys.is_directory path then (
    let files = Array.map (Filename.concat path) (Sys.readdir path) in
    Array.iter rec_rm files;
    Sys.rmdir path)
  else Sys.remove path

let delete_selection ctx to_delete =
  let term_width, _ = Term.size ctx.term in
  let msg =
    match String.concat ", " (List.map Filename.basename to_delete) with
    | str when String.length str > term_width - 30 ->
        string_of_int (List.length to_delete) ^ " items"
    | str -> str
  in
  let delete () =
    List.iter (fun file -> rec_rm file) to_delete;
    { ctx.state with selected_files = [] }
  in
  multiple_choice ctx
    ("Confirm deletion of " ^ msg ^ "? (y/n)")
    None
    [ ('y', delete); ('n', fun () -> ctx.state) ]

let rename_files ctx files =
  let rename =
    List.filter_map
      (fun file ->
        let prefix = Printf.sprintf "Rename %s:" file in
        let add = ref None in
        gather_input ctx prefix None "" (fun newname ->
            let newpath = Filename.concat (Filename.dirname file) newname in
            add := Some newpath);
        match !add with None -> None | Some newpath -> Some (file, newpath))
      files
  in
  List.iter (fun (old, newname) -> Sys.rename old newname) rename

(* TODO the below approach was causing some strange behavior and I couldn't
   figure out why, so I went with the above approach; maybe investigate this *)

(* List.iter
   (fun file ->
     let prefix = Printf.sprintf "Rename %s:" file in
     gather_input ctx prefix None "" (Sys.rename file))
   files *)

type which = NewFile | NewDirectory

let name_new_item ctx which =
  let which_str =
    match which with NewFile -> "file" | NewDirectory -> "directory"
  in
  let prefix = Printf.sprintf "Name for new %s:" which_str in
  gather_input ctx prefix None "" (fun typed_input ->
      match which with
      | NewFile -> close_out (open_out typed_input)
      | NewDirectory -> Sys.mkdir typed_input 0o755)

let create_new ctx =
  multiple_choice ctx "Create new: (f)ile, (d)irectory" None
    [
      ( 'f',
        fun () ->
          name_new_item ctx NewFile;
          ctx.state );
      ( 'd',
        fun () ->
          name_new_item ctx NewDirectory;
          ctx.state );
    ]

let filter_file_info filter_text =
  let filter_regex = Str.regexp_case_fold (".*" ^ filter_text) in
  List.filter
    (fun (_, name) -> Str.string_match filter_regex name 0)
    (read_dir ".")

let rec input_filter ctx filter_text =
  let prompt = Printf.sprintf "Regex to filter by: %s|" filter_text in
  let updated_state =
    { ctx.state with filter_text; selected_i = 0; win_start_i = 0 }
  in
  let filtered_file_info = filter_file_info filter_text in
  let updated_ctx =
    { ctx with state = updated_state; file_info = filtered_file_info }
  in
  render updated_ctx (Some prompt) None;
  match Term.event ctx.term with
  | `Key (`Escape, _) -> ctx.state
  | `Key (`Enter, _) -> updated_state
  | `Key (`ASCII c, _) -> input_filter ctx (filter_text ^ String.make 1 c)
  | `Key (`Backspace, _) ->
      let str =
        String.sub filter_text 0 (max 0 (String.length filter_text - 1))
      in
      input_filter ctx str
  | _ -> input_filter ctx filter_text

let data_dir =
  Filename.concat
    (Option.value
       ~default:(Filename.concat (Sys.getenv "HOME") ".local/share")
       (Sys.getenv_opt "XDG_DATA_HOME"))
    "omen"

let read_data_file name =
  let in_chan = open_in (Filename.concat data_dir name) in
  let rec read_lines chan lines =
    try
      let line = input_line chan in
      read_lines chan (line :: lines)
    with End_of_file ->
      close_in chan;
      List.rev lines
  in
  let lines = read_lines in_chan [] in
  List.map
    (fun x ->
      let i = String.index x ':' in
      let name = String.sub x 0 i in
      let value = String.sub x (i + 1) (String.length x - (i + 1)) in
      (name, value))
    lines

let write_to_data_file data name =
  let out_chan = open_out (Filename.concat data_dir name) in
  let rec write_lines lines =
    match lines with
    | [] -> ()
    | (name, value) :: rest ->
        output_string out_chan (Printf.sprintf "%s:%s\n" name value);
        write_lines rest
  in
  write_lines data;
  close_out out_chan

let info_img info delim =
  let keys_len =
    List.fold_left (fun m (key, _) -> max m (String.length key)) 0 info + 1
  in
  let imgs =
    List.map
      (fun (keys, action) ->
        let keys_img =
          I.hsnap ~align:`Right keys_len (I.string A.(fg lightcyan) keys)
        in
        I.(keys_img <|> string A.empty (delim ^ action)))
      info
  in
  I.vcat imgs

let set_bookmark ctx =
  let bookmarks = read_data_file "bookmarks" in
  let cwd = Sys.getcwd () in
  let unbookmark () =
    let bookmarks = List.filter (fun (_, dir) -> dir <> cwd) bookmarks in
    write_to_data_file bookmarks "bookmarks";
    ctx.state
  in
  if List.exists (fun (_, value) -> value = cwd) bookmarks then
    (* TODO maybe replace gather_input with returning state *)
    ignore
    @@ multiple_choice ctx "Unbookmark this directory? (y/n)" None
         [ ('y', unbookmark); ('n', fun () -> ctx.state) ]
  else
    let dir_name = Filename.basename (Sys.getcwd ()) in
    let above_prompt_img =
      match bookmarks with
      | [] -> None
      | _ ->
          Some
            I.(
              string A.empty "Existing bookmarks:"
              <-> void 1 1 <-> info_img bookmarks " -> ")
    in

    gather_input ctx "Bookmark name: " above_prompt_img dir_name
      (fun typed_input ->
        let bookmarks =
          (typed_input, Sys.getcwd ())
          :: List.filter
               (fun (name, _) -> name <> typed_input)
               (read_data_file "bookmarks")
        in
        write_to_data_file bookmarks "bookmarks")

let goto_bookmark ctx =
  let bookmarks = read_data_file "bookmarks" in
  let rec recurse prefix =
    let above_prompt_img = Some (info_img bookmarks " -> ") in
    gather_input ctx prefix above_prompt_img "" (fun typed_input ->
        match List.find_opt (fun (name, _) -> name = typed_input) bookmarks with
        | None -> recurse "Bookmark not found; goto bookmark:"
        | Some (_, dir) -> Sys.chdir dir)
  in
  match bookmarks with
  | [] -> notification_prompt ctx "No bookmarks have been set. " None
  | _ -> recurse "Goto bookmark:"

let show_properties ctx (classification, selected_file) =
  let file_path = Filename.concat (Sys.getcwd ()) selected_file in
  let stats = Unix.stat selected_file in

  let perm_str = perm_str stats.st_perm classification in
  let perm_str =
    Printf.sprintf "%s  uid: %d  gid: %d" perm_str stats.st_uid stats.st_gid
  in

  let atime_str = time_str (Unix.localtime stats.st_atime) in
  let mtime_str = time_str (Unix.localtime stats.st_mtime) in
  let ctime_str = time_str (Unix.localtime stats.st_ctime) in

  let file_type_norm = cmd_output_line "file" [ "-b"; selected_file ] in
  let file_type_mime = cmd_output_line "file" [ "-bi"; selected_file ] in
  let file_type = Printf.sprintf "%s; %s" file_type_norm file_type_mime in
  let file_kind =
    match stats.st_kind with
    | Unix.S_REG -> "Regular file"
    | Unix.S_DIR -> "Directory"
    | Unix.S_CHR -> "Character device"
    | Unix.S_BLK -> "Block device"
    | Unix.S_LNK -> "Symbolic link"
    | Unix.S_FIFO -> "Pipe"
    | Unix.S_SOCK -> "Socket"
  in

  let info =
    [
      ("Path", file_path);
      ("Size", string_of_int stats.st_size);
      ("Permissions", perm_str);
      ("Accessed", atime_str);
      ("Modified", mtime_str);
      ("Status changed", ctime_str);
      ("Kind", file_kind);
      ("Type", file_type);
      ("Number of links", string_of_int stats.st_nlink);
    ]
  in
  let img = info_img info "  " in
  notification_prompt ctx "" (Some img)

let new_open_with_program ctx =
  gather_input ctx "Program:" None "" (fun program ->
      let nicknames = read_data_file "openwith" in
      let above_prompt_img = Some (info_img nicknames " -> ") in
      gather_input ctx "Nickname for this program:" above_prompt_img ""
        (fun nickname ->
          let data =
            (nickname, program)
            :: List.filter (fun (name, _) -> name <> nickname) nicknames
          in
          write_to_data_file data "openwith"))

let delete_open_with_program ctx =
  let nicknames = read_data_file "openwith" in
  let above_prompt_img = Some (info_img nicknames " -> ") in
  gather_input ctx "Nickname of program to remove:" above_prompt_img ""
    (fun nickname ->
      let data = List.filter (fun (name, _) -> name <> nickname) nicknames in
      write_to_data_file data "openwith")

let open_with_configured ctx selected_file =
  let nicknames = read_data_file "openwith" in
  let above_prompt_img = Some (info_img nicknames " -> ") in

  let rec recurse prompt =
    gather_input ctx prompt above_prompt_img "" (fun nickname ->
        match List.find_opt (fun (name, _) -> name = nickname) nicknames with
        | None -> recurse "Not a valid nickname; open with:"
        | Some (_, program) ->
            ignore @@ Sys.command @@ program ^ " " ^ selected_file)
  in

  if nicknames = [] then
    notification_prompt ctx "No open-with programs have been configured yet. "
      None
  else recurse "Open with:"

let open_with ctx selected_file =
  let info =
    [
      ("d", "xdg-open; open with default program");
      ("o", "Open with chosen program");
      ("", "");
      ("c", "Open with configured program");
      ("n", "Register new program to configured open-with list");
      ("x", "Delete program from configured open-with list");
    ]
  in
  let above_prompt_img = Some (info_img info "  ") in
  multiple_choice ctx "Select an option" above_prompt_img
    [
      ( 'd',
        fun () ->
          ignore @@ Sys.command @@ "xdg-open " ^ selected_file;
          ctx.state );
      ( 'o',
        fun () ->
          gather_input ctx "Open with:" None "" (fun program ->
              ignore @@ Sys.command @@ program ^ " " ^ selected_file);
          ctx.state );
      ( 'c',
        fun () ->
          open_with_configured ctx selected_file;
          ctx.state );
      ( 'n',
        fun () ->
          new_open_with_program ctx;
          ctx.state );
      ( 'x',
        fun () ->
          delete_open_with_program ctx;
          ctx.state );
    ]

let enter_command ctx =
  gather_input ctx "$" None "" (fun typed_input ->
      ignore @@ Sys.command typed_input)
