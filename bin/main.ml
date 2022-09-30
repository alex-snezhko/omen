open Notty
open Notty_unix

let read_dir dir =
  let cwd = Sys.getcwd () in
  let dir = Filename.concat cwd dir in
  let contents = Array.to_list (Sys.readdir dir) in
  let contents =
    List.sort
      (fun x y ->
        String.compare (String.lowercase_ascii x) (String.lowercase_ascii y))
      contents
  in
  let dirs, files =
    List.partition (fun x -> Sys.is_directory @@ Filename.concat dir x) contents
  in
  List.map (fun x -> (true, x)) dirs @ List.map (fun x -> (false, x)) files

let enter_dir selected_i =
  let file_info = read_dir "." in
  let is_dir, file = List.nth file_info selected_i in
  if is_dir then Sys.chdir file

let curr_dir_img () =
  let dir = Sys.getcwd () in
  let home = Option.value ~default:"" (Sys.getenv_opt "HOME") in
  let dir =
    if String.starts_with ~prefix:home dir then
      let bi = String.length home in
      let ei = String.length dir - bi in
      "~" ^ String.sub dir bi ei
    else dir
  in
  let parents = Filename.dirname dir in
  let curr_dir = Filename.basename dir in
  I.(string A.(fg lightblue) parents <|> string A.(fg white) curr_dir)

let file_info_img selected_file =
  let stats = Unix.stat selected_file in
  let k = 1024 in
  let m = k * k in
  let g = m * k in
  let t = g * k in
  let size_str =
    match stats.st_size with
    | size when size < k -> string_of_int size ^ "B"
    | size when size < m -> string_of_int (size / k) ^ "K"
    | size when size < g -> string_of_int (size / m) ^ "M"
    | size when size < t -> string_of_int (size / g) ^ "G"
    | size -> string_of_int (size / t) ^ "T"
  in

  let perm = stats.st_perm in
  let usr_perm = (perm land 0o700) lsr 6 in
  let grp_perm = (perm land 0o070) lsr 3 in
  let wrld_perm = perm land 0o007 in
  let perm_str perm =
    let r = if perm land 0o4 <> 0 then "r" else "-" in
    let w = if perm land 0o2 <> 0 then "w" else "-" in
    let x = if perm land 0o1 <> 0 then "x" else "-" in
    r ^ w ^ x
  in
  let perm_str = perm_str usr_perm ^ perm_str grp_perm ^ perm_str wrld_perm in
  let dir_prefix = if Sys.is_directory selected_file then "d" else "-" in
  let perm_str = dir_prefix ^ perm_str in

  let mtime = Unix.localtime stats.st_mtime in
  let str n = (if n < 10 then "0" else "") ^ string_of_int n in
  let time_str =
    string_of_int (mtime.tm_year + 1900)
    ^ "-" ^ str mtime.tm_mon ^ "-" ^ str mtime.tm_mday ^ " " ^ str mtime.tm_hour
    ^ ":" ^ str mtime.tm_min
  in

  I.(
    string A.(fg lightblue) perm_str
    <|> string A.empty ("  " ^ time_str ^ "  " ^ size_str))

let dir_contents_img file_info selected_file selected_files =
  let make_img (is_dir, name) =
    let open A in
    let cwd = Sys.getcwd () in
    let is_selected = List.mem (cwd ^ "/" ^ name) selected_files in
    let fg, bg =
      match (name = selected_file, is_dir) with
      | true, true -> (fg black, bg blue)
      | true, false -> (fg black, bg white)
      | false, true -> (fg blue, empty)
      | false, false -> (fg white, empty)
    in
    (* let fg =
         fg
         @@ if name = selected_file then black else if is_dir then blue else white
       in
       let bg =
         bg
         @@ if name = selected_file then if is_dir then blue else white else black
       in *)
    let attr = fg ++ bg in
    let prefix =
      if is_selected then I.(string A.(fg green) "> ") else I.empty
    in
    I.(prefix <|> string attr name)
  in
  let imgs = List.map make_img file_info in
  List.fold_left I.( <-> ) I.empty imgs

let preview_img selected_file term_height =
  let is_dir = Sys.is_directory selected_file in
  if is_dir then dir_contents_img (read_dir selected_file) "" []
  else
    let stdout, stdin =
      Unix.open_process_args "/usr/bin/file"
        [| "file"; "-b"; "--mime-encoding"; selected_file |]
    in
    close_out stdin;
    let is_binary = input_line stdout = "binary" in
    close_in stdout;
    let rec read_lines chan lines n =
      if n = 0 then List.rev lines
      else
        try
          let line = input_line chan in
          read_lines chan (line :: lines) (n - 1)
        with End_of_file ->
          close_in chan;
          List.rev lines
    in

    let lines =
      if is_binary then [ "--- binary file ---" ]
      else
        let chan = open_in selected_file in
        read_lines chan [] term_height
    in
    let line_imgs = List.map (I.string A.empty) lines in
    List.fold_left I.( <-> ) I.empty line_imgs

let main_content_img file_info selected_file selected_files term_size =
  let term_width, term_height = term_size in
  let left_panel =
    I.hsnap ~align:`Left (term_width / 2)
      (dir_contents_img file_info selected_file selected_files)
  in
  let right_panel = preview_img selected_file (term_height - 2) in
  I.(vsnap ~align:`Top (term_height - 2) @@ left_panel <|> right_panel)

type action =
  | EnterDir
  | ExitDir
  | NextFile
  | PrevFile
  | GotoTop
  | GotoBottom
  | Select
  | SelectAll
  | UnselectAll
  | Delete
  | CopyHere
  | MoveHere
  | Rename
  | NoAction

let render term confirm_prompt (selected_i, selected_files) =
  let file_info = read_dir "." in
  let _, selected_file = List.nth file_info selected_i in
  let dir_line = curr_dir_img () in
  let main_content =
    main_content_img file_info selected_file selected_files (Term.size term)
  in
  let bottom_line =
    match confirm_prompt with
    | Some msg -> I.(string A.empty msg)
    | None -> file_info_img selected_file
  in
  Term.image term I.(dir_line <-> main_content <-> bottom_line)

let copy_file input_name output_name =
  let input_chan = open_in_bin input_name in
  let output_chan = open_out_bin output_name in
  let buf_size = 4096 in

  let rec copy_bytes () =
    let buf = Bytes.create buf_size in
    match input input_chan buf 0 buf_size with
    | 0 -> ()
    | bytes_written ->
        output output_chan buf 0 bytes_written;
        copy_bytes ()
  in

  copy_bytes ();
  close_in input_chan;
  close_out output_chan

let rec update term action ((selected_i, selected_files) as state) =
  let file_info = read_dir "." in
  let is_dir, selected_file = List.nth file_info selected_i in
  let tot_num = List.length file_info in
  let new_state =
    match action with
    | EnterDir ->
        if is_dir then (
          enter_dir selected_i;
          (0, selected_files))
        else state
    | ExitDir ->
        Sys.chdir "..";
        (0, selected_files)
    | NextFile -> ((selected_i + 1) mod tot_num, selected_files)
    | PrevFile -> ((selected_i - 1 + tot_num) mod tot_num, selected_files)
    | GotoTop -> (0, selected_files)
    | GotoBottom -> (tot_num - 1, selected_files)
    | Select ->
        let file = Filename.concat (Sys.getcwd ()) selected_file in
        let selected_files =
          if List.mem file selected_files then
            List.filter (( <> ) file) selected_files
          else file :: selected_files
        in
        (selected_i, selected_files)
    | SelectAll ->
        let cwd = Sys.getcwd () in
        let curr_dir_file_paths =
          List.map (fun (_, name) -> Filename.concat cwd name) file_info
        in
        let without_curr_dir =
          List.filter
            (fun x -> not @@ List.mem x curr_dir_file_paths)
            selected_files
        in

        (* if everything in this directory is already selected then unselect it *)
        let all_selected =
          List.for_all (fun x -> List.mem x selected_files) curr_dir_file_paths
        in
        if all_selected then (selected_i, without_curr_dir)
        else
          let selected_files = without_curr_dir @ curr_dir_file_paths in
          (selected_i, selected_files)
    | UnselectAll -> (selected_i, [])
    | Delete -> delete_selection term state
    | CopyHere ->
        List.iter (fun x -> copy_file x (Filename.basename x)) selected_files;
        (selected_i, [])
    | MoveHere ->
        List.iter (fun x -> Sys.rename x (Filename.basename x)) selected_files;
        (selected_i, [])
    | Rename ->
        let to_rename =
          match selected_files with
          | [] -> [ selected_file ]
          | _ -> selected_files
        in
        rename_files term to_rename "" state;
        (selected_i, [])
    | _ -> state
  in

  render term None new_state;
  main_loop term new_state

and main_loop term state =
  match Term.event term with
  | `Key ((`Escape | `ASCII 'q'), _) -> ()
  | `Key ((`Arrow `Up | `ASCII 'k'), _) -> update term PrevFile state
  | `Key ((`Arrow `Down | `ASCII 'j'), _) -> update term NextFile state
  | `Key (`ASCII 'g', _) -> update term GotoTop state
  | `Key (`ASCII 'G', _) -> update term GotoBottom state
  | `Key ((`Arrow `Left | `ASCII 'h'), _) -> update term ExitDir state
  | `Key ((`Arrow `Right | `ASCII 'l' | `Enter), _) ->
      update term EnterDir state
  | `Key ((`Delete | `ASCII 'x'), _) -> update term Delete state
  | `Key (`ASCII 'm', _) -> update term MoveHere state
  | `Key (`ASCII 'p', _) -> update term CopyHere state
  | `Key (`ASCII 'r', _) -> update term Rename state
  | `Key (`ASCII 's', _) -> update term Select state
  | `Key (`ASCII 'a', _) -> update term SelectAll state
  | `Key (`ASCII 'u', _) -> update term UnselectAll state
  | _ -> update term NoAction state

and delete_selection term ((selected_i, selected_files) as state) =
  let term_width, _ = Term.size term in
  let msg =
    match String.concat ", " selected_files with
    | str when String.length str > term_width - 30 ->
        string_of_int (List.length selected_files) ^ " items"
    | str -> str
  in
  render term (Some ("Confirm deletion of " ^ msg ^ "? (y/n)")) state;
  match Term.event term with
  | `Key (`ASCII 'y', _) ->
      let to_delete =
        match selected_files with
        | [] ->
            let file_info = read_dir "." in
            let _, selected_file = List.nth file_info selected_i in
            [ selected_file ]
        | _ -> selected_files
      in
      List.iter (fun file -> Sys.remove file) to_delete;
      (selected_i, [])
  | `Key ((`ASCII 'n' | `Escape), _) -> state
  | _ -> delete_selection term state

and rename_files term files typed_input state =
  match files with
  | [] -> ()
  | file :: rest -> (
      let prompt =
        Printf.sprintf "New name for%s: %s|" (Filename.basename file)
          typed_input
      in
      render term (Some prompt) state;
      match Term.event term with
      | `Key (`Escape, _) -> ()
      | `Key (`Enter, _) ->
          Sys.rename file typed_input;
          rename_files term rest "" state
      | `Key (`ASCII c, _) ->
          rename_files term files (typed_input ^ String.make 1 c) state
      | _ -> rename_files term files typed_input state)

let () =
  let term = Term.create () in
  (* let term_width, term_height = Term.size term in *)
  update term NoAction (0, []);
  Term.release term
