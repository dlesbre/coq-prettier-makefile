module LS_Set = Set.Make (struct
  type t = Location.t * string list

  let compare = compare
end)

let wait = 0.5

type compiling = {
  file : Location.file;
  status : Utils.status;
  start_time : float;
}

type compile_result = {
  filename : Location.file;
  real : float;
  _user : float;
  _sys : float;
  mem : int;
}

type line =
  | COQC of Location.file
  | COQTEST of Location.file
  | Done of compile_result
  | Make of string
  | Error of Location.t (* locs *)
  | TestError of Location.file (* file *)
  | COQDEP of string
  | CLEAN
  | COQ_MAKEFILE of string
  | PRETTY_TABLE of string
  | Unknown of string

type either = Left of Location.t | Right of Location.file

type state = {
  building : compiling list;
  (* For test, output is compared AFTER compilation, so they aren't displayed right away
     we keep their time (float) and printed status here *)
  built : (Utils.status * compile_result * float * bool) list;
  error : (either * string list) option;
  (* Coq error if location, Test error if string *)
  seen : LS_Set.t;
  printed : bool;
  folders : string list; (* folders of recursive makefiles *)
}

let initial_state =
  {
    building = [];
    seen = LS_Set.empty;
    error = None;
    printed = false;
    built = [];
    folders = [];
  }

let is_prefix prefix line = Future.string_starts_with ~prefix line
let ( let* ) x f = match x with Some t -> t | None -> f ()
let trim_start = Str.regexp {|[ \t\n\r]*["']?[ \t\n\r]*|}

let rec list_last = function
  | [] -> None
  | x :: [] -> Some x
  | _ :: xs -> list_last xs

let coqc folders x = COQC (Location.make_file x folders)

let matches_coqc folders line =
  try Scanf.sscanf line "COQC %s" (fun s -> Some (coqc folders s))
  with Scanf.Scan_failure _ -> (
    match Utils.str2argv line with
    | "coqc" :: l
    | "time" :: "coqc" :: l
    | "time" :: "-f" :: _ :: "coqc" :: l
    | "command" :: "time" :: "coqc" :: l
    | "command" :: "time" :: "-f" :: _ :: "coqc" :: l ->
        Option.map (coqc folders) (list_last l)
    | _ -> None)

let parse_test line state =
  try
    Scanf.sscanf line "--- %s " (fun file ->
        let file = Location.make_file file state.folders in
        let name = Filename.chop_extension (Location.pretty_file file) in
        match
          List.find_opt
            (fun (_, x, _, _) ->
              Filename.chop_extension (Location.pretty_file x.filename) = name)
            state.built
        with
        | Some _ -> Some file
        | None -> None)
  with Scanf.Scan_failure _ -> None

let parse_line line state =
  let trimed = Str.replace_first trim_start "" line in
  (* stop if return some t, else continue *)
  let* () =
    Option.map (fun x -> Error x) (Location.string2loc line state.folders)
  in
  let* () = Option.map (fun x -> TestError x) (parse_test line state) in
  if is_prefix "COQDEP" line then
    COQDEP (Str.replace_first (Str.regexp "COQDEP[ \t\n\r]+") "" line)
  else if line = "CLEAN" then CLEAN
  else if is_prefix "coq_makefile" trimed then COQ_MAKEFILE line
  else if is_prefix "Time |" trimed || is_prefix "After |" trimed then
    PRETTY_TABLE line
  else if is_prefix "make" trimed then Make line
  else
    let* () = matches_coqc state.folders line in
    try
      Scanf.sscanf line "COQTEST %s" (fun s ->
          COQTEST (Location.make_file s state.folders))
    with Scanf.Scan_failure _ -> (
      try
        Scanf.sscanf line "%s (real: %f, user: %f, sys: %f, mem: %d ko)"
          (fun filename real _user _sys mem ->
            let filename = Filename.chop_extension filename ^ ".v" in
            let filename = Location.make_file filename state.folders in
            Done { filename; real; _user; _sys; mem })
      with Scanf.Scan_failure _ -> Unknown line)

let print_current state =
  if not state.printed then (
    let l = List.length state.building in
    (match l with
    | 0 -> ()
    | _ ->
        Utils.print_separator ();
        List.iter
          (fun { file; status; start_time } ->
            Utils.print_file_line
              (Location.pretty_file ~extension:[ ".v" ] file)
              status
              (Unix.time () -. start_time)
              None)
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

let rec list_rm_assoc file = function
  | [] -> (None, [])
  | t :: q ->
      if Location.equal_file t.file file then (Some t, q)
      else
        let elem, list = list_rm_assoc file q in
        (elem, t :: list)

let rec update_status file status = function
  | [] -> [ { file; status; start_time = Unix.time () } ]
  | t :: q ->
      if Location.equal_file t.file file then
        { t with status = Utils.max_status status t.status } :: q
      else t :: update_status file status q

(* strip's diff "+++" from test error messages*)
let rec strip_ppp = function
  | [] -> []
  | msg :: [] as l ->
      if Future.string_starts_with ~prefix:"+++ " msg then [] else l
  | msg :: msgs -> msg :: strip_ppp msgs

let rec print_test force time = function
  | [] -> []
  | ((status, compiling, t, printed) as info) :: l ->
      if (not printed) && (force || t +. wait < time) then (
        Utils.print_file_line
          (Location.pretty_file ~extension:[ ".vo"; ".v" ] compiling.filename)
          status compiling.real (Some compiling.mem);
        (status, compiling, t, true) :: print_test force time l)
      else info :: print_test force time l

let resolve_error state =
  let open ANSITerminal in
  let state =
    { state with built = print_test false (Unix.time ()) state.built }
  in
  match state.error with
  | None -> state
  | Some (Left loc, msg) ->
      if LS_Set.mem (loc, msg) state.seen then { state with error = None }
      else
        let state = { state with seen = LS_Set.add (loc, msg) state.seen } in
        let file = Location.get_file loc in
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
  | Some (Right file, msg) ->
      let msg = strip_ppp msg in
      let msg = List.fold_left (fun acc m -> m ^ "\n" ^ acc) "" msg in
      printf [ Bold; red ] "Test Failed";
      printf [] ": %s\n" (Location.pretty_file file);
      Utils.print_error msg [ red ];
      printf [] "\n";
      {
        state with
        error = None;
        built =
          List.map
            (fun (status, c, i, b) ->
              if
                Filename.chop_extension (Location.pretty_file c.filename)
                = Filename.chop_extension (Location.pretty_file file)
              then (Utils.S_TestFail, c, i, b)
              else (status, c, i, b))
            state.built;
      }

let is_done = ref false
let todo = Queue.create ()

let print_line state = function
  | COQC file ->
      let state = resolve_error state in
      let state =
        {
          state with
          building = update_status file Utils.S_Compiling state.building;
        }
      in
      state
  | COQTEST file ->
      let state = resolve_error state in
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
  | Make m -> (
      let state = resolve_error state in
      try
        Scanf.sscanf m "make[%d]: Entering directory '%s@'" (fun _ s ->
            { state with folders = s :: state.folders })
      with Scanf.Scan_failure _ ->
        (* ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.yellow ] "make";
           ANSITerminal.printf [] "%s\n" (String.sub m 4 (String.length m - 4)); *)
        state)
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
      let file =
        Location.pretty_file ~extension:[ ".vo"; ".glob"; ".v" ] d.filename
      in
      let v_file = Location.make_file (file ^ ".v") state.folders in
      let status, building = list_rm_assoc v_file state.building in
      let status =
        match status with
        | None -> Utils.S_Ok
        | Some s -> Utils.status_done s.status
      in
      let time = Unix.time () in
      if status != S_TestOk then (
        Utils.print_file_line file status d.real (Some d.mem);
        { state with building; built = (status, d, time, true) :: state.built })
      else
        { state with building; built = (status, d, time, false) :: state.built }
  | Unknown u -> (
      match state.error with
      | Some (l, s) -> { state with error = Some (l, u :: s) }
      | None ->
          ANSITerminal.printf [] "%s\n" u;
          state)
  | Error l ->
      let state = resolve_error state in
      { state with error = Some (Left l, []) }
  | TestError s ->
      let state = resolve_error state in
      { state with error = Some (Right s, []) }

type acc = {
  time : float;
  max_mem : int;
  nb_ok : int;
  nb_testok : int;
  nb_warnings : int;
  nb_error : int;
  nb_testfail : int;
}

let folder acc (status, res, _, _) =
  let time = acc.time +. res.real in
  let max_mem = max acc.max_mem res.mem in
  let acc = { acc with time; max_mem } in
  let open Utils in
  match status with
  | S_Ok -> { acc with nb_ok = acc.nb_ok + 1 }
  | S_TestOk -> { acc with nb_testok = acc.nb_testok + 1 }
  | S_Warning -> { acc with nb_warnings = acc.nb_warnings + 1 }
  | S_Error -> { acc with nb_error = acc.nb_error + 1 }
  | S_TestFail -> { acc with nb_testfail = acc.nb_testfail + 1 }
  | _ -> acc

let print_final is_ok time state =
  let open ANSITerminal in
  let state = { state with built = print_test true 0.0 state.built } in
  let state = resolve_error state in
  printf [ Bold ] "\n%s in %s\n"
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
        nb_testfail = 0;
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
  let s, e =
    if final.nb_error != 0 then (
      printf [ Bold ] "%s" s;
      printf [ Bold; red ] "%d errors" final.nb_error;
      (", ", "\n"))
    else (s, e)
  in
  let e =
    if final.nb_testfail != 0 then (
      printf [ Bold ] "%s" s;
      printf [ Bold; red ] "%d test failed" final.nb_testfail;
      "\n")
    else e
  in
  printf [ Bold ] "%s" e;
  if final.time > 0. then
    printf [ Bold ] "Total time %s, max memory used: %s\n"
      (Utils.pretty_time final.time)
      (Utils.pretty_size ~padding:0 final.max_mem);
  state
