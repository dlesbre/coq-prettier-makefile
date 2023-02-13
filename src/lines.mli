type line
type state

(* ==== string utility functions ==== *)
val is_prefix : string -> string -> bool
val split_path : string -> string list

(* ==== Line and state manipulation ==== *)
val parse_line : string -> line
val print_line : state -> line -> state
val resolve_error : state -> state

val print_current : state -> state
(** Prints currently compiling files *)

val clear_current : state -> state
(** Clears the printed currently compiling files, call before printing anything *)

val initial_state : state
val is_done : bool ref
val todo : string Queue.t
