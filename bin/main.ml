open Notty
open Notty_unix

(* let square = "\xe2\x96\xaa" *)

(* let rec sierp n =
  if n > 1 then
    let ss = sierp (pred n) in I.(ss <-> (ss <|> ss))
  else I.(string A.(fg magenta) square |> hpad 1 0) *)

let read_dir () =
  let contents = Array.to_list (Sys.readdir (Sys.getcwd ())) in
  (* List.partition Sys.is_directory (Array.to_list contents) *)
  (* let with_dir_info = List.map (fun x -> (Sys.is_directory x, x)) contents in *)
  List.partition Sys.is_directory contents


type action = EnterDir | NextFile | PrevFile | ShrinkLeft | GrowLeft | NoAction

let directory_data (_, selected_i) =
  let dirs, files = read_dir () in
  let file_info = List.map (fun x -> (true, x)) dirs @ List.map (fun x -> (false, x)) files in
  let img_attr selected is_dir =
    let open A in
    let fg = fg @@ if selected then black else if is_dir then blue else white in
    let bg = bg @@ if selected then (if is_dir then blue else white) else black in
    fg ++ bg
  in
  let imgs = List.mapi (fun i (is_dir, name) -> I.string (img_attr (i = selected_i) is_dir) name) file_info in
  List.fold_left I.(<->) I.empty imgs

let () =
  let term = Term.create () in
  let (width, _) = Term.size term in
  let rec update term action state =
    Term.image term (directory_data state);
    loop term state
  and loop term (left_width, selected_i as state) =
    match Term.event term with
    | `Key (`Escape, _)       -> ()
    | `Key ((`Arrow `Up | `ASCII 'k'), _)    -> update term PrevFile state
    | `Key ((`Arrow `Down | `ASCII 'j'), _)  -> update term NextFile state
    | `Key ((`Arrow `Left | `ASCII 'h'), _)  -> update term ShrinkLeft state
    | `Key ((`Arrow `Right | `ASCII 'l'), _) -> update term GrowLeft state
    | `Key (`Enter, _)                       -> update term EnterDir state
    | `Resize _                              -> update term NoAction state
    | _                                      -> loop term state
  in
  update term NoAction (width / 2, 0);
  Term.release term
