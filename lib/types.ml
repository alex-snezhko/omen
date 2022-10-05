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
  | Bookmark
  | ViewBookmarks
  | ViewProperties
  | OpenWith
  | EnterCommand
  | NoAction
  | Help
  | Exit

type state = {
  selected_i : int;
  win_start_i : int;
  selected_files : string list;
  filter_text : string;
}

(* classifications slightly modified from how `ls` colors files *)
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

type context = {
  term : Notty_unix.Term.t;
  file_info : (file_classification * string) list;
  state : state;
}

(* type extra_render_info = NormalRender | WithPrompt of string * I.t

   type render_details = {
     ctx : context;
     render_info : extra_render_info;
   } *)
