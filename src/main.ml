type file = string
type status = S_Ok | S_Error | S_Warning

let print_status = function
  | S_Ok -> ANSITerminal.printf [ ANSITerminal.green ] "OK     "
  | S_Error -> ANSITerminal.printf [ ANSITerminal.red ] "ERROR  "
  | S_Warning -> ANSITerminal.printf [ ANSITerminal.magenta ] "WARNING"

let max_status l r =
  match (l, r) with
  | S_Error, _ | _, S_Error -> S_Error
  | S_Warning, _ | _, S_Warning -> S_Warning
  | S_Ok, S_Ok -> S_Ok

module LS_Set = Set.Make (struct
  type t = Location.t * string list

  let compare = compare
end)

type state = {
  building : (file * status) list;
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
  | Unknown of string

let ( let* ) x f = match x with Some t -> t | None -> f ()

let parse_line line =
  let* () = Option.map (fun x -> Error (x, [])) (Location.string2loc line) in
  let* () = if line = "COQDEP VFILES" then Some COQDEP else None in
  let* () = if line = "CLEAN" then Some CLEAN else None in
  let* () =
    if String.starts_with ~prefix:"make" line then Some (Make line) else None
  in
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
    ANSITerminal.printf [ ANSITerminal.Bold ] "Compiling %d files:\n" l;
    List.iter
      (fun (s, _) ->
        ANSITerminal.printf [ ANSITerminal.Bold ] " - ";
        ANSITerminal.printf [ ANSITerminal.blue ] "%s\n" s)
      state.building;
    flush stdout;
    { state with printed = true })
  else state

let clear_current state =
  if state.printed then (
    ANSITerminal.move_bol ();
    ANSITerminal.move_cursor 0 (-List.length state.building - 1);
    ANSITerminal.erase ANSITerminal.Below;
    { state with printed = false })
  else state

let pretty_time t =
  let t' = abs (int_of_float t) in
  let hours = t' / 3600 in
  let t' = t' mod 3600 in
  let minutes = (t' / 60) - (60 * hours) in
  Format.sprintf "%02d:%02d:%02d" hours minutes (t' mod 60)

let time_breaks =
  [
    (1., ANSITerminal.green);
    (10., ANSITerminal.cyan);
    (60., ANSITerminal.blue);
    (150., ANSITerminal.yellow);
    (300., ANSITerminal.magenta);
  ]

let print_time time =
  let rec aux = function
    | [] -> ANSITerminal.red
    | (s, c) :: q -> if time <= s then c else aux q
  in
  ANSITerminal.printf [ aux time_breaks ] "%s" (pretty_time time)

let pretty_size value =
  let base = 1000. in
  let rec aux value suffix =
    if value < base then Format.sprintf "%3.1f " value ^ List.hd suffix
    else aux (value /. 1000.) (List.tl suffix)
  in
  aux (float_of_int value) [ "ko"; "Mo"; "Go"; "To"; "Po"; "Eo"; "Zo" ]

let size_breaks =
  [
    (10_000, ANSITerminal.green);
    (100_000, ANSITerminal.cyan);
    (300_000, ANSITerminal.blue);
    (500_000, ANSITerminal.yellow);
    (1_000_000, ANSITerminal.magenta);
  ]

let print_size size =
  let rec aux = function
    | [] -> ANSITerminal.red
    | (s, c) :: q -> if size <= s then c else aux q
  in
  ANSITerminal.printf [ aux size_breaks ] "%s" (pretty_size size)

let contains s1 s2 =
  let re = Str.regexp_case_fold s2 in
  try
    ignore (Str.search_forward re s1 0);
    true
  with Not_found -> false

let is_error msg = contains msg "Error" || contains msg "Command exited"

let resolve_error state =
  match state.error with
  | None -> state
  | Some (loc, msg) ->
      if LS_Set.mem (loc, msg) state.seen then state
      else
        let state = { state with seen = LS_Set.add (loc, msg) state.seen } in
        let file =
          Location.pretty_filename ~extension:".v" (Location.get_file loc)
        in
        let msg = List.fold_left (fun acc m -> m ^ "\n" ^ acc) "" msg in
        let color, text, status =
          if is_error msg then (ANSITerminal.red, "Error", S_Error)
          else (ANSITerminal.magenta, "Warning", S_Warning)
        in
        ANSITerminal.printf [ ANSITerminal.Bold ] "%s\n"
          (Location.loc2string loc);
        Location.show_loc loc [ color ];
        ANSITerminal.printf [ ANSITerminal.Bold; color ] "%s" text;
        ANSITerminal.printf [] ":\n | %s\n"
          (Str.global_replace (Str.regexp "\n") "\n | " (String.trim msg));
        let old_status = List.assoc file state.building in
        {
          state with
          building =
            (file, max_status old_status status)
            :: List.remove_assoc file state.building;
        }

let print_line state = function
  | COQC file ->
      let state = resolve_error state in
      let file = Location.pretty_filename ~extension:".v" file in
      let state = { state with building = state.building @ [ (file, S_Ok) ] } in
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
  | Done d ->
      let state = resolve_error state in
      let file = Location.pretty_filename ~extension:".vo" d.file in
      let status = List.assoc file state.building in
      print_time d.real;
      ANSITerminal.printf [] " | ";
      print_size d.mem;
      ANSITerminal.printf [] " | ";
      print_status status;
      ANSITerminal.printf [] " | %s\n" file;
      { state with building = List.remove_assoc file state.building }
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

let rec main ic state =
  try
    let line = input_line ic in
    if line = str_end then
      let state = clear_current state in
      resolve_error state
    else
      let line = parse_line line in
      let state = clear_current state in
      let state = print_line state line in
      let state = print_current state in
      main ic state
  with
  | End_of_file -> main ic state
  | Sys.Break -> state

let _ =
  Sys.catch_break true;
  let argv = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1)) in
  let in_c, out_c, err_c =
    Unix.open_process_full
      (Filename.quote_command "make" argv ^ "; echo " ^ str_end)
      (Unix.environment ())
  in
  let state =
    { building = []; seen = LS_Set.empty; error = None; printed = false }
  in
  let _ = main in_c state in
  Unix.close_process_full (in_c, out_c, err_c)
