open Types

let empty_stats : Unix.stats =        
{ st_dev = 0;
  st_ino = 0;
  st_kind = S_REG;
  st_perm = 0;
  st_nlink = 0;
  st_uid = 0;
  st_gid = 0;
  st_rdev = 0;
  st_size = 0;
  st_atime = 0.0;
  st_mtime = 0.0;
  st_ctime = 0.0;
  }

let perm_str perm classification =
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
  let prefix =
    match classification with Directory -> "d" | Symlink -> "l" | _ -> "-"
  in
  prefix ^ perm_str

let time_str (time : Unix.tm) =
  let str n = (if n < 10 then "0" else "") ^ string_of_int n in
  string_of_int (time.tm_year + 1900)
  ^ "-" ^ str time.tm_mon ^ "-" ^ str time.tm_mday ^ " " ^ str time.tm_hour
  ^ ":" ^ str time.tm_min

let cmd_output_line cmd args =
  let stdout = Unix.open_process_args_in cmd (Array.of_list (cmd :: args)) in
  let output = input_line stdout in
  close_in stdout;
  output

let read_dir dir =
  let cwd = Sys.getcwd () in
  let dir = Filename.concat cwd dir in
  let contents = ( try (Sys.readdir dir) with Sys_error _ ->
    [|"Permission Denied"|] ) |> Array.to_list
  in
  let contents =
    List.sort
      (fun x y ->
        String.compare (String.lowercase_ascii x) (String.lowercase_ascii y))
      contents
  in
  let result =
    List.map
      (fun name ->
        let path = Filename.concat dir name in
        let stats = try ( Unix.lstat path ) with Unix.Unix_error _ ->
          empty_stats
        in
        let classification =
          match stats.st_kind with
          | Unix.S_LNK -> Symlink
          | Unix.S_BLK | Unix.S_CHR -> Device
          | Unix.S_FIFO -> Pipe
          | Unix.S_DIR -> Directory
          | Unix.S_SOCK -> Socket
          | _ ->
              let ext = Filename.extension name in
              let open File_extensions in

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
