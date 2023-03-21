type t

val string2loc : string -> t option
val loc2string : t -> string
val get_file : t -> string

val show_loc : t -> ANSITerminal.style list -> unit
(** Prints file at location (if it exists/can be opened) *)

val pretty_filename : ?extension:string list -> string -> string
(** Remove leading ./, removes extension if supplied and matches *)
