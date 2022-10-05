# omen
A simple TUI file manager that exists mostly because I wanted to try out OCaml. It is somewhat of a mashup of [nnn](https://github.com/jarun/nnn) and [ranger](https://github.com/ranger/ranger) functionally, with a few changes to match my personal taste.

## Overview
An overview of some of omen's features
- 2-pane interface; left: contents of directory; right: preview of selected item
- Opening programs with program of choice (and saving commonly used programs for quick access)
- Standard file manipulations: moving, renaming, copying, deleting, creating new files
- Bookmarking directories for quick access
- Filtering directory contents

Omen is controlled using the keyboard mostly, and has vim-like keybinds. `?` opens the help dialog, where all of omen's commands are displayed. For batch operations, files can be 'selected' to then be operated on. For example, selecting several files in one directory, moving into another directory and then pasting will copy all of the files into that directory.

## Screenshots

![normal screenshot](screenshots/normal.png)
![help dialog screenshot](screenshots/helpdialog.png)
![deleting selections](screenshots/selectionpreview.png)
![open with](screenshots/openwithpreview.png)
![bookmarks](screenshots/bookmarkspreview.png)
![filtering](screenshots/filteringpreview.png)
![showing file stats](screenshots/filestatspreview.png)

