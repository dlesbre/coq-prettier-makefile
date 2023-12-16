type status =
  | S_Ok
  | S_Error
  | S_Warning
  | S_Compiling
  | S_Testing
  | S_TestOk
  | S_TestFail

val max_status : status -> status -> status
(** Combine status information: Ok and Error is Error *)

val status_done : status -> status
(** Switch status from compiling to done *)

(* ==== Printer functions with fancy formatting ==== *)
(* val print_status : status -> unit
   val print_time : float -> unit
   val print_size : int option -> unit
   val print_file : string -> unit *)

val pretty_time : float -> string
(** Returns time in HH:MM:SS format *)

val pretty_size : ?padding:int -> int -> string
(** Returns rounded size with unit [pretty size 2048] is ["  2.0 Ko"].
    padding defaults to 5 *)

val print_error : string -> ANSITerminal.style list -> unit
(** Print an error message, indented by "  | " in the given style *)

val print_file_line : string -> status -> float -> int option -> unit
(** Print "HH:MM:SS | MEM | STATUS | File" line
    string is the file, float is the time, int option the memory used *)

val print_separator : unit -> unit
(** Print "---------|-----|--------|-----", the separator *)

val str2argv : string -> string list
(** Basic parsing into shell args
    ["foo bar with\\ space \"quoted\""] -> ["foo"; "bar"; "with space"; "quoted"] *)
