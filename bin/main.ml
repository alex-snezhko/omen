open Notty
open Notty_unix

let read_dir dir =
  let cwd = Sys.getcwd () in
  let dir = cwd ^ "/" ^ dir in
  let contents = Array.to_list (Sys.readdir dir) in
  let contents = List.sort (fun x y -> String.compare (String.lowercase_ascii x) (String.lowercase_ascii y)) contents in
  let dirs, files =
    List.partition (fun x -> Sys.is_directory @@ dir ^ "/" ^ x) contents
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
  let parts = String.split_on_char '/' dir in
  let parents, curr_dir =
    match List.rev parts with
    | [] -> assert false
    | curr_dir :: rest ->
        let parents = String.concat "/" (List.rev rest) in
        ((if List.length parts = 1 then parents else parents ^ "/"), curr_dir)
  in
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
    let (fg, bg) = match (name = selected_file, is_dir) with
    | (true, true) -> (fg black, bg blue)
    | (true, false) -> (fg black, bg white)
    | (false, true) -> (fg blue, empty)
    | (false, false) -> (fg white, empty) in
    (* let fg =
      fg
      @@ if name = selected_file then black else if is_dir then blue else white
    in
    let bg =
      bg
      @@ if name = selected_file then if is_dir then blue else white else black
    in *)
    let attr = fg ++ bg in
    let prefix = if is_selected then I.(string A.(fg green) "> ") else I.empty in
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
  let (term_width, term_height) = term_size in
  let left_panel =
    I.hsnap ~align:`Left (term_width / 2)
      (dir_contents_img file_info selected_file selected_files)
  in
  let right_panel = preview_img selected_file (term_height - 2) in
  I.(vsnap ~align:`Top (term_height - 2) @@ left_panel <|> right_panel)

type action = EnterDir | ExitDir | NextFile | PrevFile | Select | Delete | NoAction

let render term confirm_prompt (selected_i, selected_files) =
  let file_info = read_dir "." in
  let _, selected_file = List.nth file_info selected_i in
  let dir_line = curr_dir_img () in
  let main_content = main_content_img file_info selected_file selected_files (Term.size term) in
  let bottom_line = match confirm_prompt with
  | Some msg -> I.(string A.empty msg)
  | None -> file_info_img selected_file in
  Term.image term I.(dir_line <-> main_content <-> bottom_line)

let rec update term action (selected_i, selected_files as state) =
  let file_info = read_dir "." in
  let _, selected_file = List.nth file_info selected_i in
  let tot_num = List.length file_info in
  let new_state = match action with
  | EnterDir -> enter_dir selected_i; (0, selected_files)
  | ExitDir -> Sys.chdir ".."; (0, selected_files)
  | NextFile -> ((selected_i + 1) mod tot_num, selected_files)
  | PrevFile -> ((selected_i - 1 + tot_num) mod tot_num, selected_files)
  | Select ->
    let file = Sys.getcwd () ^ "/" ^ selected_file in
    let selected_files = if List.mem file selected_files then List.filter ((<>) file) selected_files else file :: selected_files in
    (selected_i, selected_files)
  | Delete -> delete_selection term state
  | _ -> state in

  render term None new_state;
  main_loop term new_state

  (* (match action with
  | EnterDir -> enter_dir selected_i
  | ExitDir -> Sys.chdir ".."
  | _ -> ());
  let file_info = read_dir "." in
  let tot_num = List.length file_info in
  let selected_i =
    match action with
    | NextFile -> (selected_i + 1) mod tot_num
    | PrevFile -> (selected_i - 1 + tot_num) mod tot_num
    | _ -> selected_i
  in
  let _, selected_file = List.nth file_info selected_i in
  Term.image term
    I.(
      curr_dir_img ()
      <-> main_content_img file_info selected_file term_width term_height
      <-> file_info_img selected_file); *)

and main_loop term state =
  match Term.event term with
  | `Key (`Escape, _) -> ()
  | `Key ((`Arrow `Up | `ASCII 'k'), _) -> update term PrevFile state
  | `Key ((`Arrow `Down | `ASCII 'j'), _) -> update term NextFile state
  | `Key ((`Arrow `Left | `ASCII 'h'), _) -> update term ExitDir state
  | `Key ((`Arrow `Right | `ASCII 'l' | `Enter), _) ->
      update term EnterDir state
  | `Key ((`Delete | `ASCII 'r'), _) -> update term Delete state
  | `Key (`ASCII 's', _) -> update term Select state
  | `Resize _ -> update term NoAction state
  | _ -> main_loop term state

and delete_selection term (selected_i, selected_files as state) =
  let (term_width, _) = Term.size term in
  let msg = match String.concat ", " selected_files with
  | str when String.length str > term_width - 30 -> string_of_int (List.length selected_files) ^ " items"
  | str -> str in
  render term (Some ("Confirm deletion of " ^ msg ^  "? (y/n)")) state;
  match Term.event term with
  | `Key (`ASCII 'y', _) -> 
    let to_delete = match selected_files with
    | [] ->
      let file_info = read_dir "." in
      let (_, selected_file) = List.nth file_info selected_i in
      [selected_file]
    | _ -> selected_files in
    List.iter (fun file -> Sys.remove file) to_delete;
    (selected_i, [])
  | `Key ((`ASCII 'n' | `Escape), _) -> state
  | _ -> delete_selection term state

let () =
  let term = Term.create () in
  (* let term_width, term_height = Term.size term in *)
  update term NoAction (0, []);
  Term.release term
