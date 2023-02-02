module LS_Set = Set.Make (struct
  type t = Location.t * string list

  let compare = compare
end)

module SMap = Map.Make (String)

type compiling = { file : string; status : Utils.status; start_time : float }

type state = {
  building : compiling list;
  error : (Location.t * string list) option;
  seen : LS_Set.t;
  printed : bool;
}

type line =
  | COQC of string
  | Done of {
      file : string;
      real : float;
      user : float;
      sys : float;
      mem : int;
    }
  | Make of string
  | Error of Location.t * string list
  | COQDEP
  | CLEAN
  | COQ_MAKEFILE of string
  | PRETTY_TABLE of string
  | Unknown of string

let parse_line line =
  let trimed = String.trim line in
  (* stop if return some t, else continue *)
  let ( let* ) x f = match x with Some t -> t | None -> f () in
  let if_ b c = if b then Some c else None in
  let* () = Option.map (fun x -> Error (x, [])) (Location.string2loc line) in
  let* () = if_ (line = "COQDEP VFILES") COQDEP in
  let* () = if_ (line = "CLEAN") CLEAN in
  let* () =
    if_ (String.starts_with ~prefix:"coq_makefile" trimed) (COQ_MAKEFILE line)
  in
  let* () =
    if_
      (String.starts_with ~prefix:"Time |" trimed
      || String.starts_with ~prefix:"After |" trimed)
      (PRETTY_TABLE line)
  in
  let* () = if_ (String.starts_with ~prefix:"make" trimed) (Make line) in
  let* () =
    try Scanf.sscanf line "COQC %s" (fun s -> Some (COQC s))
    with Scanf.Scan_failure _ -> None
  in
  let* () =
    try
      Scanf.sscanf line "%s (real: %f, user: %f, sys: %f, mem: %d ko)"
        (fun file real user sys mem ->
          Some (Done { file; real; user; sys; mem }))
    with Scanf.Scan_failure _ -> None
  in
  Unknown line

let print_current state =
  if not state.printed then (
    let l = List.length state.building in
    (match l with
    | 0 -> ()
    | _ ->
        Utils.print_separator ();
        List.iter
          (fun { file; status; start_time } ->
            Utils.print_file_line file status (Unix.time () -. start_time) None)
          state.building);
    flush stdout;
    { state with printed = true })
  else state

let clear_current state =
  if state.printed then (
    (match List.length state.building with
    | 0 -> ()
    | _ ->
        ANSITerminal.move_bol ();
        ANSITerminal.move_cursor 0 (-List.length state.building - 1);
        ANSITerminal.erase ANSITerminal.Below);
    { state with printed = false })
  else state

let contains s1 s2 =
  let re = Str.regexp_case_fold s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let is_error msg = contains msg "Error" || contains msg "Command exited"

let rec list_rm_assoc file = function
  | [] -> (None, [])
  | t :: q ->
      if t.file = file then (Some t, q)
      else
        let elem, list = list_rm_assoc file q in
        (elem, t :: list)

let rec update_status file status = function
  | [] -> [ { file; status; start_time = Unix.time () } ]
  | t :: q ->
      if t.file = file then
        { t with status = Utils.max_status status t.status } :: q
      else t :: update_status file status q

let resolve_error state =
  let open ANSITerminal in
  match state.error with
  | None -> state
  | Some (loc, msg) ->
      if LS_Set.mem (loc, msg) state.seen then { state with error = None }
      else
        let state = { state with seen = LS_Set.add (loc, msg) state.seen } in
        let file =
          Location.pretty_filename ~extension:".v" (Location.get_file loc)
        in
        let msg = List.fold_left (fun acc m -> m ^ "\n" ^ acc) "" msg in
        let color, text, status =
          if is_error msg then (red, "Error", Utils.S_Error)
          else (magenta, "Warning", Utils.S_Warning)
        in
        printf [ Bold ] "%s\n" (Location.loc2string loc);
        Location.show_loc loc [ color ];
        printf [ Bold; color ] "%s" text;
        printf [] ":\n";
        Utils.print_error msg [ color ];
        printf [] "\n";
        {
          state with
          error = None;
          building = update_status file status state.building;
        }

let is_done = ref false
let todo = Queue.create ()

let print_line state = function
  | COQC file ->
      let state = resolve_error state in
      let file = Location.pretty_filename ~extension:".v" file in
      let state =
        {
          state with
          building = update_status file Utils.S_Compiling state.building;
        }
      in
      state
  | COQDEP ->
      let state = resolve_error state in
      ANSITerminal.printf
        [ ANSITerminal.Bold; ANSITerminal.green ]
        "Finding dependencies with COQDEP\n";
      state
  | CLEAN ->
      let state = resolve_error state in
      ANSITerminal.printf
        [ ANSITerminal.Bold; ANSITerminal.green ]
        "Cleaning build files\n";
      state
  | Make _m ->
      let state = resolve_error state in
      (* ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.yellow ] "make";
         ANSITerminal.printf [] "%s\n" (String.sub m 4 (String.length m - 4)); *)
      state
  | COQ_MAKEFILE _line ->
      let state = resolve_error state in
      ANSITerminal.printf
        [ ANSITerminal.Bold; ANSITerminal.green ]
        "Creating makefile with coq_makefile\n";
      state
  | PRETTY_TABLE _ ->
      let state = resolve_error state in
      is_done := true;
      Queue.clear todo;
      state
  | Done d ->
      let state = resolve_error state in
      let file = Location.pretty_filename ~extension:".vo" d.file in
      let status, building = list_rm_assoc file state.building in
      let status =
        match status with
        | None -> Utils.S_Ok
        | Some s -> Utils.max_status Utils.S_Ok s.status
      in
      Utils.print_file_line file status d.real (Some d.mem);
      { state with building }
  | Unknown u -> (
      match state.error with
      | Some (l, s) -> { state with error = Some (l, u :: s) }
      | None ->
          ANSITerminal.printf [] "%s\n" u;
          state)
  | Error (l, s) ->
      let state = resolve_error state in
      { state with error = Some (l, s) }

let str_end = "coq-prettier-makefile-done"
let update_time = 1.

let rec fetch_input ic =
  try
    if not !is_done then
      let line = input_line ic in
      if line = str_end then is_done := true
      else (
        Queue.add line todo;
        fetch_input ic)
  with
  | End_of_file ->
      Unix.sleepf update_time;
      fetch_input ic
  | Sys.Break -> Thread.exit ()

let rec mainloop state start =
  try
    if Queue.is_empty todo then (
      let state = clear_current state in
      if !is_done then (
        let state = resolve_error state in
        ANSITerminal.printf [ ANSITerminal.Bold ] "Process ended in %s\n"
          (Utils.pretty_time (Unix.time () -. start));
        state)
      else
        let state = print_current state in
        Unix.sleepf update_time;
        mainloop state start)
    else
      let line = Queue.take todo in
      let line = parse_line line in
      let state = clear_current state in
      let state = print_line state line in
      let state = print_current state in
      mainloop state start
  with
  | End_of_file ->
      Unix.sleepf update_time;
      mainloop state start
  | Sys.Break ->
      let state = clear_current state in
      let state = resolve_error state in
      ANSITerminal.printf [ ANSITerminal.Bold ] "INTERRUPTED in %s\n"
        (Utils.pretty_time (Unix.time () -. start));
      state

(* Found list on https://coq.inria.fr/refman/practical-tools/utilities.html *)
let coq_makefile_targets =
  [
    "pre-all";
    "post-all";
    "install-extra";
    "install-doc";
    "uninstall";
    "uninstall-doc";
    "clean";
    "cleanall";
    "archclean";
    "merlin-hook";
    "pretty-timed";
    "print-pretty-timed-diff";
    "pretty-timed-before";
    "pretty-timed-after";
    "print-pretty-single-time-diff";
    "only";
  ]

let smap_safe_add k v m = if SMap.mem k m then m else SMap.add k v m

(* Add all partial path to the map of matchings
   so foo/bar/t.v will be matched by
   foo/bar/t bar/t and t (with or without .v) *)
let rec build_path target smap = function
  | [] -> smap
  | f :: fs ->
      let partial = String.concat "/" (f :: fs) in
      let smap = smap_safe_add partial target smap in
      let smap =
        smap_safe_add (Filename.remove_extension partial) target smap
      in
      build_path target smap fs

let add_v_file file smap =
  if Filename.check_suffix file ".v" then
    let target = file ^ "o" in
    build_path target smap (String.split_on_char '/' file)
  else smap

let rec parse_coqproject lexbuf smap =
  try
    match Coq_project.token lexbuf with
    | Arg s -> parse_coqproject lexbuf (add_v_file s smap)
    | Eof -> smap
  with _ -> smap

let parse_coqproject file =
  if Sys.file_exists file then
    try
      let ic = open_in file in
      let lexbuf = Lexing.from_channel ic in
      parse_coqproject lexbuf SMap.empty
    with _ -> SMap.empty
  else SMap.empty

let rec parse_argv smap targets argv = function
  | [] -> (List.rev targets, List.rev argv)
  | arg :: args -> (
      if String.starts_with ~prefix:"-" arg || List.mem arg coq_makefile_targets
      then parse_argv smap targets (arg :: argv) args
      else
        match SMap.find_opt arg smap with
        | Some s -> parse_argv smap (s :: targets) argv args
        | None -> parse_argv smap targets (arg :: argv) args)

let get_argv () =
  let argv = Array.to_list Sys.argv in
  (* strip command name *)
  let argv = "TIMED=1" :: List.tl argv in
  let smap = parse_coqproject "./_CoqProject" in
  let targets, argv = parse_argv smap [] [] argv in
  Format.sprintf "TGTS=\"%s\"" (String.concat " " targets) :: argv

let main () =
  Sys.catch_break true;
  let argv = get_argv () in
  let in_c, out_c, err_c =
    Unix.open_process_full
      (Filename.quote_command "make" argv ^ "; echo " ^ str_end)
      (Unix.environment ())
  in
  let state =
    { building = []; seen = LS_Set.empty; error = None; printed = false }
  in
  let _ = Thread.create fetch_input in_c in
  let _ = mainloop state (Unix.time ()) in
  (* Rings terminal bell *)
  ANSITerminal.printf [] "\007";
  Unix.close_process_full (in_c, out_c, err_c)

let _ = main ()
