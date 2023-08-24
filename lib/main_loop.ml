open Types
open Notty_unix
open Term_render
open Command_handlers

let show_help ctx =
  let info =
    [
      ("q", "Exit omen");
      ("", "");
      ("h, Lt", "Go to parent directory");
      ("j, Dn, ScrlDn", "Go down");
      ("k, Up, ScrlUp", "Go up");
      ("l, Rt, Enter", "Enter selected directory/open file");
      ("o", "Open with program");
      ("g", "Go to first entry in directory");
      ("G", "Go to last entry in directory");
      ("", "");
      ("Space, s", "Select this item");
      ("a", "Select all items in current directory");
      ("u", "Unselect all selected items");
      ("", "");
      ("x, Del", "Delete selected file");
      ("m", "Move selected files to current directory");
      ("p", "Copy selected files to current directory");
      ("r", "Rename selected file(s)");
      ("n", "Create new file/directory");
      ("", "");
      ("b", "(Un)bookmark current directory");
      ("B", "View all bookmarks");
      ("f", "View file properties");
      ("/", "Filter by regex");
      ("$", "Enter a shell command");
      ("?", "View this help screen");
    ]
  in
  notification_prompt ctx "" (Some (info_img info "  "))

let get_action event =
  match event with
  | `Key (`ASCII 'q', _) -> Exit
  | `Key ((`Arrow `Up | `ASCII 'k'), _) | `Mouse (`Press (`Scroll `Up), _, _) ->
      PrevFile
  | `Key ((`Arrow `Down | `ASCII 'j'), _) | `Mouse (`Press (`Scroll `Down), _, _)
    ->
      NextFile
  | `Key ((`Arrow `Left | `ASCII 'h'), _) -> ExitDir
  | `Key ((`Arrow `Right | `ASCII 'l' | `Enter), _) -> EnterDir
  | `Key (`ASCII 'g', _) -> GotoTop
  | `Key (`ASCII 'G', _) -> GotoBottom
  | `Key ((`Delete | `ASCII 'x'), _) -> Delete
  | `Key (`ASCII 'm', _) -> MoveHere
  | `Key (`ASCII 'p', _) -> CopyHere
  | `Key (`ASCII 'r', _) -> Rename
  | `Key (`ASCII 'n', _) -> CreateNew
  | `Key (`ASCII 's', _) -> Select
  | `Key (`ASCII ' ', _) -> Select
  | `Key (`ASCII 'a', _) -> SelectAll
  | `Key (`ASCII 'u', _) -> UnselectAll
  | `Key (`ASCII '/', _) -> Filter
  | `Key (`ASCII 'b', _) -> Bookmark
  | `Key (`ASCII 'B', _) -> ViewBookmarks
  | `Key (`ASCII 'f', _) -> ViewProperties
  | `Key (`ASCII 'o', _) -> OpenWith
  | `Key (`ASCII '$', _) -> EnterCommand
  | `Key (`ASCII '?', _) -> Help
  | _ -> NoAction

(* the "main loop" of the program *)
let rec update term action
    ({ selected_i; win_start_i; selected_files; filter_text } as state) =
  if action = Exit then ()
  else
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

    let ctx = { term; file_info; state } in
    let new_state =
      match action with
      | EnterDir -> (
          match selected_file_opt with
          | Some (classification, selected_file) ->
              if classification = Directory then (
                Sys.chdir selected_file;
                { state with selected_i = 0; win_start_i = 0; filter_text = "" })
              else
                let _ = Sys.command ("xdg-open " ^ selected_file) in
                state
          | None -> state)
      | ExitDir ->
          Sys.chdir "..";
          { state with selected_i = 0; win_start_i = 0; filter_text = "" }
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
            List.for_all
              (fun x -> List.mem x selected_files)
              curr_dir_file_paths
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
          delete_selection ctx to_delete
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
          rename_files ctx to_rename;
          { state with selected_files = [] }
      | CreateNew -> create_new ctx
      | Filter -> input_filter ctx filter_text
      | Bookmark ->
          set_bookmark ctx;
          state
      | ViewBookmarks ->
          goto_bookmark ctx;
          state
      | ViewProperties ->
          (match selected_file_opt with
          | None -> ()
          | Some selected -> show_properties ctx selected);
          state
      | OpenWith -> (
          match selected_file_opt with
          | None -> state
          | Some (_, selected_file) -> open_with ctx selected_file)
      | EnterCommand ->
          enter_command ctx;
          state
      | Help ->
          show_help ctx;
          state
      | NoAction -> state
      | Exit -> assert false
    in

    let file_info = filter_file_info new_state.filter_text in
    let new_ctx = { ctx with file_info; state = new_state } in
    render new_ctx None None;

    let action = get_action (Term.event term) in
    update term action new_state

let init_files () =
  ignore @@ Sys.command ("mkdir -p " ^ data_dir);
  ignore @@ Sys.command ("touch " ^ Filename.concat data_dir "bookmarks");
  ignore @@ Sys.command ("touch " ^ Filename.concat data_dir "openwith")

let run () =
  let term = Term.create () in
  init_files ();
  update term NoAction
    { selected_i = 0; win_start_i = 0; selected_files = []; filter_text = "" };
  Term.release term
