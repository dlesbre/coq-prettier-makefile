type line
type state

(* ==== string utility functions ==== *)
val is_prefix : string -> string -> bool
val split_path : string -> string list

(* ==== Line and state manipulation ==== *)
val parse_line : string -> line

val print_line : state -> line -> state
(** Print result of current line, and add it's info to the state *)

val print_final : bool -> float -> state -> state
(** Prints a final widget with totals/max, float is time since started
    the bool is true if terminated normally, false for interrupted (eg Ctrl+C) *)

val print_current : state -> state
(** Prints currently compiling files *)

val clear_current : state -> state
(** Clears the printed currently compiling files, call before printing anything *)

val initial_state : state
val is_done : bool ref
val todo : string Queue.t
