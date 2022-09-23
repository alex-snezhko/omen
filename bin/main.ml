open Notty
open Notty_unix

let read_dir dir =
  let cwd = Sys.getcwd () in
  let dir = cwd ^ "/" ^ dir in
  let contents = Array.to_list (Sys.readdir dir) in
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

let dir_contents_img file_info selected_file =
  let make_img (is_dir, name) =
    let open A in
    let fg =
      fg
      @@ if name = selected_file then black else if is_dir then blue else white
    in
    let bg =
      bg
      @@ if name = selected_file then if is_dir then blue else white else black
    in
    let attr = fg ++ bg in
    I.string attr name
  in
  let imgs = List.map make_img file_info in
  List.fold_left I.( <-> ) I.empty imgs

let preview_img selected_file term_height =
  let is_dir = Sys.is_directory selected_file in
  if is_dir then dir_contents_img (read_dir selected_file) ""
  else
    let (stdout, stdin) =
      Unix.open_process_args "/usr/bin/file" [| "file"; "-b"; "--mime-encoding"; selected_file |]
    in
    close_out stdin;
    let is_binary = input_line stdout = "binary" in
    close_in stdout;
    let rec read_lines chan lines n =
      if n = 0 then List.rev lines else
      try
        let line = input_line chan in
        read_lines chan (line :: lines) (n - 1)
      with End_of_file -> close_in chan; List.rev lines in

    let lines = if is_binary then ["--- binary file ---"] else (
      let chan = open_in selected_file in
      read_lines chan [] term_height
    ) in
    let line_imgs = List.map (I.string A.empty) lines in
    List.fold_left I.(<->) I.empty line_imgs

let main_content_img file_info selected_file term_width term_height =
  let left_panel =
    I.hsnap ~align:`Left (term_width / 2)
      (dir_contents_img file_info selected_file)
  in
  let right_panel = preview_img selected_file (term_height - 2) in
  I.(vsnap ~align:`Top (term_height - 2) @@ left_panel <|> right_panel)

type action = EnterDir | ExitDir | NextFile | PrevFile | NoAction

let () =
  let term = Term.create () in
  let (term_width, term_height) = Term.size term in
  let rec update term action selected_i =
    (match action with
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
        <-> file_info_img selected_file);
    loop term selected_i
  and loop term state =
    match Term.event term with
    | `Key (`Escape, _) -> ()
    | `Key ((`Arrow `Up | `ASCII 'k'), _) -> update term PrevFile state
    | `Key ((`Arrow `Down | `ASCII 'j'), _) -> update term NextFile state
    | `Key ((`Arrow `Left | `ASCII 'h'), _) -> update term ExitDir state
    | `Key ((`Arrow `Right | `ASCII 'l' | `Enter), _) ->
        update term EnterDir state
    | `Resize _ -> update term NoAction state
    | _ -> loop term state
  in
  update term NoAction 0;
  Term.release term
