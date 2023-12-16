type t
(** A File location (filename + line number + col number) *)

type file
(** A filename with absolute/relative paths, and a pretty path *)

val make_file : string -> string list -> file
(** Create a file from string. string_list is the list of CWD the file can be in *)

val string2loc : string -> string list -> t option
(** Parse a location string, as printed by Coq error messages *)

val loc2string : t -> string
(** Parse a location string, as printed by Coq error messages
    e.g. 'File "foo.v", lines 2-3, characters 1-5' *)

val get_file : t -> file
(** Get the file included in the location *)

val show_loc : t -> ANSITerminal.style list -> unit
(** Prints file at location (if it exists/can be opened) *)

val pretty_file : ?extension:string list -> file -> string
(** Remove leading ./, removes extension if supplied and matches *)

val equal_file : file -> file -> bool
(** Equality between filenames *)
