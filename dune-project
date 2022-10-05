(lang dune 3.3)

(name omen)

(generate_opam_files true)

(source
 (github alex-snezhko/omen))

(authors "Alex Snezhko")

(maintainers "Alex Snezhko")

(license LICENSE)

(package
 (name omen)
 (synopsis "A simple TUI file manager")
 (description "A TUI file manager functionally similar to nnn and ranger")
 (depends ocaml dune notty))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
