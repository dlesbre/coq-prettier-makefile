module LS_Set = Set.Make (struct
  type t = Location.t * string list

  let compare = compare
end)

type compiling = { file : string; status : Utils.status; start_time : float }

type compile_result = {
  filename : string;
  real : float;
  _user : float;
  _sys : float;
  mem : int;
}

type line =
  | COQC of string
  | COQTEST of string
  | Done of compile_result
  | Make of string
  | Error of Location.t * string list
  | COQDEP of string
  | CLEAN
  | COQ_MAKEFILE of string
  | PRETTY_TABLE of string
  | Unknown of string

type state = {
  building : compiling list;
  built : (Utils.status * compile_result) list;
  error : (Location.t * string list) option;
  seen : LS_Set.t;
  printed : bool;
}

let initial_state =
  {
    building = [];
    seen = LS_Set.empty;
    error = None;
    printed = false;
    built = [];
  }

let is_prefix prefix line = String.starts_with ~prefix line
let ( let* ) x f = match x with Some t -> t | None -> f ()
let trim_start = Str.regexp {|[ \t\n\r]*["']?[ \t\n\r]*|}

let parse_line line =
  let trimed = Str.replace_first trim_start "" line in
  (* stop if return some t, else continue *)
  let* () = Option.map (fun x -> Error (x, [])) (Location.string2loc line) in
  if is_prefix "COQDEP" line then
    COQDEP (Str.replace_first (Str.regexp "COQDEP[ \t\n\r]+") "" line)
  else if line = "CLEAN" then CLEAN
  else if is_prefix "coq_makefile" trimed then COQ_MAKEFILE line
  else if is_prefix "Time |" trimed || is_prefix "After |" trimed then
    PRETTY_TABLE line
  else if is_prefix "make" trimed then Make line
  else
    try Scanf.sscanf line "COQC %s" (fun s -> COQC s)
    with Scanf.Scan_failure _ -> (
      try Scanf.sscanf line "COQTEST %s" (fun s -> COQTEST s)
      with Scanf.Scan_failure _ -> (
        try
          Scanf.sscanf line "%s (real: %f, user: %f, sys: %f, mem: %d ko)"
            (fun filename real _user _sys mem ->
              Done { filename; real; _user; _sys; mem })
        with Scanf.Scan_failure _ -> Unknown line))

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

let split_path path =
  let split_char =
    if String.length Filename.dir_sep = 1 then Filename.dir_sep.[0] else '/'
  in
  String.split_on_char split_char path

let contains s1 s2 =
  let re = Str.regexp_case_fold s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let is_error msg = contains msg "Error" || contains msg "Command exited"

let files_match a b =
  let a = Location.pretty_filename a in
  let b = Location.pretty_filename b in
  let path_a = List.rev (split_path a) in
  let path_b = List.rev (split_path b) in
  let rec compare l r =
    match (l, r) with
    | [], [] -> true
    | [], _ -> Filename.is_relative a && not (Filename.is_relative b)
    | _, [] -> Filename.is_relative b && not (Filename.is_relative a)
    | a :: a', b :: b' -> a = b && compare a' b'
  in
  compare path_a path_b

let rec list_rm_assoc file = function
  | [] -> (None, [])
  | t :: q ->
      if files_match t.file file then (Some t, q)
      else
        let elem, list = list_rm_assoc file q in
        (elem, t :: list)

let rec update_status file status = function
  | [] -> [ { file; status; start_time = Unix.time () } ]
  | t :: q ->
      if files_match t.file file then
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
  | COQTEST file ->
      let state = resolve_error state in
      let file = Location.pretty_filename ~extension:".v" file in
      let state =
        {
          state with
          building = update_status file Utils.S_Testing state.building;
        }
      in
      state
  | COQDEP s ->
      let state = resolve_error state in
      ANSITerminal.printf
        [ ANSITerminal.Bold; ANSITerminal.green ]
        "Finding dependencies with coqdep for %s\n" s;
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
      let file = Location.pretty_filename ~extension:".vo" d.filename in
      let status, building = list_rm_assoc file state.building in
      let status =
        match status with
        | None -> Utils.S_Ok
        | Some s -> Utils.status_done s.status
      in
      Utils.print_file_line file status d.real (Some d.mem);
      { state with building; built = (status, d) :: state.built }
  | Unknown u -> (
      match state.error with
      | Some (l, s) -> { state with error = Some (l, u :: s) }
      | None ->
          ANSITerminal.printf [] "%s\n" u;
          state)
  | Error (l, s) ->
      let state = resolve_error state in
      { state with error = Some (l, s) }

type acc = {
  time : float;
  max_mem : int;
  nb_ok : int;
  nb_testok : int;
  nb_warnings : int;
  nb_error : int;
}

let folder acc (status, res) =
  let time = acc.time +. res.real in
  let max_mem = max acc.max_mem res.mem in
  let acc = { acc with time; max_mem } in
  let open Utils in
  match status with
  | S_Ok -> { acc with nb_ok = acc.nb_ok + 1 }
  | S_TestOk -> { acc with nb_testok = acc.nb_testok + 1 }
  | S_Warning -> { acc with nb_warnings = acc.nb_warnings + 1 }
  | S_Error -> { acc with nb_error = acc.nb_error + 1 }
  | _ -> acc

let print_final is_ok time state =
  let open ANSITerminal in
  let state = resolve_error state in
  printf [ Bold ] "%s in %s\n"
    (if is_ok then "Process ended" else "INTERRUPTED")
    (Utils.pretty_time time);
  let final =
    List.fold_left folder
      {
        time = 0.;
        max_mem = 0;
        nb_ok = 0;
        nb_testok = 0;
        nb_warnings = 0;
        nb_error = 0;
      }
      state.built
  in
  let s, e =
    if final.nb_ok != 0 then (
      printf [ Bold ] "Compiled %d files" final.nb_ok;
      (", ", "\n"))
    else ("", "")
  in
  let s, e =
    if final.nb_testok != 0 then (
      printf [ Bold ] "%sRan %d tests" s final.nb_testok;
      (", ", "\n"))
    else (s, e)
  in
  let s, e =
    if final.nb_warnings != 0 then (
      printf [ Bold ] "%s" s;
      printf [ Bold; magenta ] "%d warnings" final.nb_warnings;
      (", ", "\n"))
    else (s, e)
  in
  if final.nb_error != 0 then (
    printf [ Bold ] "%s" s;
    printf [ Bold; red ] "%d errors" final.nb_error);
  printf [ Bold ] "%s" e;
  if final.time > 0. then
    printf [ Bold ] "Total time %s, max memory used: %s\n"
      (Utils.pretty_time final.time)
      (Utils.pretty_size ~padding:0 final.max_mem);
  state
