type t
type file

val make_file : string -> string list -> file
(** Prints file at location (if it exists/can be opened) *)

val string2loc : string -> string list -> t option
val loc2string : t -> string
val get_file : t -> file

val show_loc : t -> ANSITerminal.style list -> unit
(** Prints file at location (if it exists/can be opened) *)

val pretty_file : ?extension:string list -> file -> string
(** Remove leading ./, removes extension if supplied and matches *)

val equal_file : file -> file -> bool
