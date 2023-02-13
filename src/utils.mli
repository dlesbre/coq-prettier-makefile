type status = S_Ok | S_Error | S_Warning | S_Compiling | S_Testing | S_TestOk

val max_status : status -> status -> status
val status_done : status -> status

(* Printer functions with fancy formatting *)
(* val print_status : status -> unit
   val print_time : float -> unit
   val print_size : int option -> unit
   val print_file : string -> unit *)
val pretty_time : float -> string
val pretty_size : int -> string
val print_error : string -> ANSITerminal.style list -> unit
val print_file_line : string -> status -> float -> int option -> unit
val print_separator : unit -> unit
