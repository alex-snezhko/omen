open Notty
open Notty_unix

type state = {
  selected_i : int;
  win_start_i : int;
  selected_files : string list;
  filter_text : string;
}

type file_classification =
  | Directory
  | EmptyFile
  | NormalFile
  | Executable
  | Symlink
  | Graphic
  | Audio
  | Archive
  | Device
  | Pipe
  | Socket

(* classifications slightly modified from $LS_COLORS *)
let archive_extensions =
  [
    ".tar";
    ".tgz";
    ".arc";
    ".arj";
    ".taz";
    ".lha";
    ".lz4";
    ".lzh";
    ".lzma";
    ".tlz";
    ".txz";
    ".tzo";
    ".t7z";
    ".zip";
    ".z";
    ".dz";
    ".gz";
    ".lrz";
    ".lz";
    ".lzo";
    ".xz";
    ".zst";
    ".tzst";
    ".bz2";
    ".bz";
    ".tbz";
    ".tbz2";
    ".tz";
    ".deb";
    ".rpm";
    ".jar";
    ".war";
    ".ear";
    ".sar";
    ".rar";
    ".alz";
    ".ace";
    ".zoo";
    ".cpio";
    ".7z";
    ".rz";
    ".cab";
    ".wim";
    ".swm";
    ".dwm";
    ".esd";
  ]

let graphic_extensions =
  [
    ".jpg";
    ".jpeg";
    ".mjpg";
    ".mjpeg";
    ".gif";
    ".bmp";
    ".pbm";
    ".pgm";
    ".ppm";
    ".tga";
    ".xbm";
    ".xpm";
    ".tif";
    ".tiff";
    ".png";
    ".svg";
    ".svgz";
    ".mng";
    ".pcx";
    ".mov";
    ".mpg";
    ".mpeg";
    ".m2v";
    ".mkv";
    ".webm";
    ".webp";
    ".ogm";
    ".mp4";
    ".m4v";
    ".mp4v";
    ".vob";
    ".qt";
    ".nuv";
    ".wmv";
    ".asf";
    ".rm";
    ".rmvb";
    ".flc";
    ".avi";
    ".fli";
    ".flv";
    ".gl";
    ".dl";
    ".xcf";
    ".xwd";
    ".yuv";
    ".cgm";
    ".emf";
    ".ogv";
    ".ogx";
  ]

let audio_extensions =
  [
    ".aac";
    ".au";
    ".flac";
    ".m4a";
    ".mid";
    ".midi";
    ".mka";
    ".mp3";
    ".mpc";
    ".ogg";
    ".ra";
    ".wav";
    ".oga";
    ".opus";
    ".spx";
    ".xspf";
  ]

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
  (* let paths = List.map (Filename.concat dir) contents in *)
  let result =
    List.map
      (fun name ->
        let path = Filename.concat dir name in
        let stats = Unix.stat path in
        let classification =
          match stats.st_kind with
          | Unix.S_LNK -> Symlink
          | Unix.S_BLK | Unix.S_CHR -> Device
          | Unix.S_FIFO -> Pipe
          | Unix.S_DIR -> Directory
          | Unix.S_SOCK -> Socket
          | _ ->
              let ext = Filename.extension name in

              if List.mem ext archive_extensions then Archive
              else if List.mem ext graphic_extensions then Graphic
              else if List.mem ext audio_extensions then Audio
              else if stats.st_perm land 0o111 <> 0 then Executable
              else if stats.st_size = 0 then EmptyFile
              else NormalFile
        in

        (classification, name))
      contents
  in
  let dirs, files = List.partition (fun (c, _) -> c = Directory) result in
  dirs @ files

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
      let usr_perm = (perm land 0o700) lsr 6 in
      let grp_perm = (perm land 0o070) lsr 3 in
      let wrld_perm = perm land 0o007 in
      let p perm =
        let r = if perm land 0o4 <> 0 then "r" else "-" in
        let w = if perm land 0o2 <> 0 then "w" else "-" in
        let x = if perm land 0o1 <> 0 then "x" else "-" in
        r ^ w ^ x
      in
      let perm_str = p usr_perm ^ p grp_perm ^ p wrld_perm in
      let dir_prefix = if classification = Directory then "d" else "-" in
      let perm_str = dir_prefix ^ perm_str in

      let mtime = Unix.localtime stats.st_mtime in
      let str n = (if n < 10 then "0" else "") ^ string_of_int n in
      let time_str =
        string_of_int (mtime.tm_year + 1900)
        ^ "-" ^ str mtime.tm_mon ^ "-" ^ str mtime.tm_mday ^ " "
        ^ str mtime.tm_hour ^ ":" ^ str mtime.tm_min
      in

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

let dir_contents_img file_info selected_file win_start_i selected_files
    term_height =
  let make_img (classification, name) =
    let cwd = Sys.getcwd () in
    let is_selected = List.mem (Filename.concat cwd name) selected_files in
    let color, extra =
      match classification with
      | Directory -> (A.blue, A.(st bold))
      | EmptyFile -> (A.gray 15, A.empty)
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
  let win_end_i = win_start_i + term_height in
  let files_to_show =
    List.filteri (fun i _ -> i >= win_start_i && i <= win_end_i) file_info
  in
  let imgs = List.map make_img files_to_show in
  List.fold_left I.( <-> ) I.empty imgs

let preview_img (classification, selected_file) term_height =
  if classification = Directory then
    dir_contents_img (read_dir selected_file) "" 0 [] term_height
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

let main_content_img file_info { selected_i; win_start_i; selected_files; _ }
    term_size =
  let term_width, term_height = term_size in
  let img =
    match List.nth_opt file_info selected_i with
    | Some ((_, selected_file) as f) ->
        (* let _, selected_file = List.nth file_info selected_i in *)
        let left_panel =
          I.hsnap ~align:`Left (term_width / 2)
            (dir_contents_img file_info selected_file win_start_i selected_files
               term_height)
        in
        let right_panel = preview_img f (term_height - 2) in
        I.(left_panel <|> right_panel)
    | None -> I.empty
  in
  I.(vsnap ~align:`Top (term_height - 2) img)

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
  | CreateNew
  | Filter
  | NoAction

let render term file_info confirm_prompt
    ({ selected_i; filter_text; _ } as state) =
  let dir_line = curr_dir_img () in
  let main_content = main_content_img file_info state (Term.size term) in
  let bottom_line =
    match confirm_prompt with
    | Some msg -> I.(string A.empty msg)
    | None -> file_info_img file_info selected_i filter_text
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

let rec delete_selection term file_info to_delete state =
  let term_width, _ = Term.size term in
  let msg =
    match String.concat ", " (List.map Filename.basename to_delete) with
    | str when String.length str > term_width - 30 ->
        string_of_int (List.length to_delete) ^ " items"
    | str -> str
  in
  render term file_info (Some ("Confirm deletion of " ^ msg ^ "? (y/n)")) state;
  match Term.event term with
  | `Key (`ASCII 'y', _) ->
      List.iter (fun file -> Sys.remove file) to_delete;
      { state with selected_files = [] }
  | `Key ((`ASCII 'n' | `Escape), _) -> state
  | _ -> delete_selection term file_info to_delete state

let rec rename_files term file_info files typed_input state =
  match files with
  | [] -> ()
  | file :: rest -> (
      let prompt =
        Printf.sprintf "Rename %s: %s|" (Filename.basename file) typed_input
      in
      render term file_info (Some prompt) state;
      match Term.event term with
      | `Key (`Escape, _) -> ()
      | `Key (`Enter, _) ->
          Sys.rename file typed_input;
          rename_files term file_info rest "" state
      | `Key (`ASCII c, _) ->
          rename_files term file_info files
            (typed_input ^ String.make 1 c)
            state
      | `Key (`Backspace, _) ->
          let str =
            String.sub typed_input 0 (max 0 (String.length typed_input - 1))
          in
          rename_files term file_info files str state
      | _ -> rename_files term file_info files typed_input state)

type which = NewFile | NewDirectory

let rec name_new_item term file_info which typed_input state =
  let which_str =
    match which with NewFile -> "file" | NewDirectory -> "directory"
  in
  let prompt = Printf.sprintf "Name for new %s by: %s|" which_str typed_input in
  render term file_info (Some prompt) state;
  match Term.event term with
  | `Key (`Escape, _) -> ()
  | `Key (`Enter, _) -> (
      match which with
      | NewFile -> close_out (open_out typed_input)
      | NewDirectory -> Sys.mkdir typed_input 0o755)
  | `Key (`ASCII c, _) ->
      name_new_item term file_info which (typed_input ^ String.make 1 c) state
  | `Key (`Backspace, _) ->
      let str =
        String.sub typed_input 0 (max 0 (String.length typed_input - 1))
      in
      name_new_item term file_info which str state
  | _ -> name_new_item term file_info which typed_input state

let rec create_new term file_info state =
  render term file_info (Some "Create new: (f)ile, (d)irectory") state;
  match Term.event term with
  | `Key (`Escape, _) -> ()
  | `Key (`ASCII 'f', _) -> name_new_item term file_info NewFile "" state
  | `Key (`ASCII 'd', _) -> name_new_item term file_info NewDirectory "" state
  | _ -> create_new term file_info state

let filter_file_info filter_text =
  let filter_regex = Str.regexp filter_text in
  List.filter
    (fun (_, name) -> Str.string_match filter_regex name 0)
    (read_dir ".")

let rec input_filter term file_info filter_text state =
  let prompt = Printf.sprintf "Regex to filter by: %s|" filter_text in
  let updated_state =
    { state with filter_text; selected_i = 0; win_start_i = 0 }
  in
  let filtered_file_info = filter_file_info filter_text in
  render term filtered_file_info (Some prompt) updated_state;
  match Term.event term with
  | `Key (`Escape, _) -> state
  | `Key (`Enter, _) -> updated_state
  | `Key (`ASCII c, _) ->
      input_filter term file_info (filter_text ^ String.make 1 c) state
  | `Key (`Backspace, _) ->
      let str =
        String.sub filter_text 0 (max 0 (String.length filter_text - 1))
      in
      input_filter term file_info str state
  | _ -> input_filter term file_info filter_text state

let rec update term action
    ({ selected_i; win_start_i; selected_files; filter_text } as state) =
  let _, term_height = Term.size term in
  let file_info = filter_file_info filter_text in
  (* max 1 is a bit of a hack to make empty directories play nicely *)
  let tot_num = max 1 (List.length file_info) in
  let selected_file_opt = List.nth_opt file_info selected_i in

  let adjust_window new_i =
    let main_window_h = term_height - 2 in
    let new_i_in_window = new_i - win_start_i in
    let positions_shifted = new_i - selected_i in
    let min_padding = 3 in
    if
      new_i > selected_i
      && new_i_in_window >= main_window_h - min_padding
      && selected_i < tot_num - min_padding - 1
    then min (tot_num - main_window_h) (win_start_i + positions_shifted)
    else if new_i < selected_i && new_i_in_window < min_padding then
      max 0 (win_start_i + positions_shifted)
    else win_start_i
  in

  let new_state =
    match action with
    | EnterDir -> (
        match selected_file_opt with
        | Some (classification, selected_file) ->
            if classification = Directory then (
              Sys.chdir selected_file;
              { state with selected_i = 0; win_start_i = 0 })
            else
              let _ = Sys.command ("/usr/bin/nvim " ^ selected_file) in
              state
        | None -> state)
    | ExitDir ->
        Sys.chdir "..";
        { state with selected_i = 0; win_start_i = 0 }
    | NextFile ->
        let selected_i = (selected_i + 1) mod tot_num in
        let win_start_i = adjust_window selected_i in
        { state with selected_i; win_start_i }
    | PrevFile ->
        let selected_i = (selected_i - 1 + tot_num) mod tot_num in
        let win_start_i = adjust_window selected_i in
        { state with selected_i; win_start_i }
    | GotoTop ->
        let win_start_i = adjust_window 0 in
        { state with selected_i = 0; win_start_i }
    | GotoBottom ->
        let win_start_i = adjust_window (tot_num - 1) in
        { state with selected_i = tot_num - 1; win_start_i }
    | Select -> (
        match selected_file_opt with
        | Some (_, selected_file) ->
            let file = Filename.concat (Sys.getcwd ()) selected_file in
            let selected_files =
              if List.mem file selected_files then
                List.filter (( <> ) file) selected_files
              else file :: selected_files
            in
            { state with selected_files }
        | None -> state)
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
        if all_selected then { state with selected_files = without_curr_dir }
        else
          let selected_files = without_curr_dir @ curr_dir_file_paths in
          { state with selected_files }
    | UnselectAll -> { state with selected_files = [] }
    | Delete ->
        let to_delete =
          match selected_files with
          | [] ->
              Option.to_list (Option.map (fun (_, x) -> x) selected_file_opt)
          | _ -> selected_files
        in
        delete_selection term file_info to_delete state
    | CopyHere ->
        List.iter (fun x -> copy_file x (Filename.basename x)) selected_files;
        { state with selected_files = [] }
    | MoveHere ->
        List.iter (fun x -> Sys.rename x (Filename.basename x)) selected_files;
        { state with selected_files = [] }
    | Rename ->
        let to_rename =
          match selected_files with
          | [] ->
              Option.to_list (Option.map (fun (_, x) -> x) selected_file_opt)
          | _ -> selected_files
        in
        rename_files term file_info to_rename "" state;
        { state with selected_files = [] }
    | CreateNew ->
        create_new term file_info state;
        state
    | Filter -> input_filter term file_info filter_text state
    | _ -> state
  in

  let file_info = filter_file_info new_state.filter_text in
  render term file_info None new_state;
  main_loop term new_state

and main_loop term state =
  match Term.event term with
  | `Key ((`Escape | `ASCII 'q'), _) -> ()
  | `Key ((`Arrow `Up | `ASCII 'k'), _) -> update term PrevFile state
  | `Key ((`Arrow `Down | `ASCII 'j'), _) -> update term NextFile state
  | `Key ((`Arrow `Left | `ASCII 'h'), _) -> update term ExitDir state
  | `Key ((`Arrow `Right | `ASCII 'l' | `Enter), _) ->
      update term EnterDir state
  | `Key (`ASCII 'g', _) -> update term GotoTop state
  | `Key (`ASCII 'G', _) -> update term GotoBottom state
  | `Key ((`Delete | `ASCII 'x'), _) -> update term Delete state
  | `Key (`ASCII 'm', _) -> update term MoveHere state
  | `Key (`ASCII 'p', _) -> update term CopyHere state
  | `Key (`ASCII 'r', _) -> update term Rename state
  | `Key (`ASCII 'n', _) -> update term CreateNew state
  | `Key (`ASCII 's', _) -> update term Select state
  | `Key (`ASCII 'a', _) -> update term SelectAll state
  | `Key (`ASCII 'u', _) -> update term UnselectAll state
  | `Key (`ASCII '/', _) -> update term Filter state
  | _ -> update term NoAction state

let () =
  let term = Term.create () in
  update term NoAction
    { selected_i = 0; win_start_i = 0; selected_files = []; filter_text = "" };
  Term.release term
