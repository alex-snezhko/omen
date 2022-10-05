open Types
open Utils
open Notty
open Notty_unix

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
  (* let parents = Filename.dirname dir ^ Filename.dir_sep in *)
  let curr_dir = Filename.basename dir in
  let parents = Filename.chop_suffix dir curr_dir in
  I.(string A.(fg lightblue) parents <|> string A.(fg white) curr_dir)

let file_info_img file_info selected_i filter_text =
  match List.nth_opt file_info selected_i with
  | None ->
      let msg =
        if filter_text = "" then "This directory is empty"
        else "No matches found for search regex " ^ filter_text
      in
      I.(string A.(fg lightred) msg)
  | Some (classification, selected_file) ->
      let tot_num = List.length file_info in

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
      let perm_str = perm_str perm classification in

      let mtime = Unix.localtime stats.st_mtime in
      let time_str = time_str mtime in

      let file_num_str =
        string_of_int (selected_i + 1) ^ "/" ^ string_of_int tot_num ^ "  "
      in
      let filtering_notif =
        if filter_text <> "" then
          I.(string A.(fg green) ("Filtering on regex " ^ filter_text ^ "  "))
        else I.empty
      in
      I.(
        string A.(fg (gray 15)) file_num_str
        <|> filtering_notif
        <|> string A.(fg lightblue) (perm_str ^ "  ")
        <|> string A.empty (time_str ^ "  " ^ size_str))

let dir_contents_img file_info selected_file win_start_i selected_files height =
  let make_img (classification, name) =
    let cwd = Sys.getcwd () in
    let is_selected = List.mem (Filename.concat cwd name) selected_files in
    let color, extra =
      match classification with
      | Directory -> (A.lightblue, A.(st bold))
      | EmptyFile -> (A.gray 10, A.empty)
      | NormalFile -> (A.white, A.empty)
      | Executable -> (A.green, A.(st bold))
      | Symlink -> (A.cyan, A.(st bold))
      | Graphic -> (A.magenta, A.(st bold))
      | Audio -> (A.cyan, A.empty)
      | Archive -> (A.red, A.(st bold))
      | Device -> (A.yellow, A.(st bold))
      | Pipe -> (A.yellow, A.empty)
      | Socket -> (A.lightmagenta, A.(st bold))
    in
    let attr =
      A.(
        extra ++ if name = selected_file then fg black ++ bg color else fg color)
    in
    let prefix =
      if is_selected then I.(string A.(fg green) "> ") else I.empty
    in
    I.(prefix <|> string attr name)
  in
  let win_end_i = win_start_i + height in
  let files_to_show =
    List.filteri (fun i _ -> i >= win_start_i && i <= win_end_i) file_info
  in
  let imgs = List.map make_img files_to_show in
  I.vcat imgs

let preview_img (classification, selected_file) term_height =
  if classification = Directory then
    dir_contents_img (read_dir selected_file) "" 0 [] term_height
  else
    let is_binary =
      cmd_output_line "file" [ "-b"; "--mime-encoding"; selected_file ]
      = "binary"
    in
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

    if is_binary then
      let output = cmd_output_line "file" [ "-b"; selected_file ] in
      I.(
        string A.(fg lightgreen) "--- File details ---"
        <-> string A.empty output)
    else
      let chan = open_in selected_file in
      let lines = read_lines chan [] term_height in
      let line_imgs = List.map (I.string A.empty) lines in
      I.vcat line_imgs

let main_content_img file_info size
    { selected_i; win_start_i; selected_files; _ } =
  let width, height = size in
  let img =
    match List.nth_opt file_info selected_i with
    | Some ((_, selected_file) as f) ->
        (* let _, selected_file = List.nth file_info selected_i in *)
        let left_panel =
          I.hsnap ~align:`Left (width / 2)
            (dir_contents_img file_info selected_file win_start_i selected_files
               height)
        in
        let right_panel = preview_img f height in
        I.(pad ~l:1 ~r:1 left_panel <|> right_panel)
    | None -> I.empty
  in
  I.vsnap ~align:`Top height img

let render ctx confirm_prompt above_prompt_img =
  let { selected_i; filter_text; _ } = ctx.state in
  let term_width, term_height = Term.size ctx.term in
  let dir_line = curr_dir_img () in
  let main_content =
    main_content_img ctx.file_info (term_width, term_height - 2) ctx.state
  in
  let above_prompt_img =
    match above_prompt_img with
    | None -> I.empty
    | Some img ->
        I.(
          uchar A.empty (Uchar.of_int 0x2500) term_width 1 <-> img <-> void 1 1)
  in
  let main_content = I.crop ~b:(I.height above_prompt_img) main_content in
  let bottom_line =
    match confirm_prompt with
    | Some msg -> I.(string A.empty msg)
    | None -> file_info_img ctx.file_info selected_i filter_text
  in
  Term.image ctx.term
    I.(dir_line <-> main_content <-> above_prompt_img <-> bottom_line)
