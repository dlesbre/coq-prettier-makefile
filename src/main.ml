type file = string
type status = S_Ok | S_Error | S_Warning

let print_status = function
  | S_Ok -> ANSITerminal.printf [ ANSITerminal.green ] "OK     "
  | S_Error -> ANSITerminal.printf [ ANSITerminal.red ] "ERROR  "
  | S_Warning -> ANSITerminal.printf [ ANSITerminal.magenta ] "WARNING"

let max_status l r =
  match l with
  | None -> r
  | Some l -> (
      match (l, r) with
      | S_Error, _ | _, S_Error -> S_Error
      | S_Warning, _ | _, S_Warning -> S_Warning
      | S_Ok, S_Ok -> S_Ok)

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
  | COQ_MAKEFILE of string
  | PRETTY_TABLE of string
  | Unknown of string

let parse_line line =
  (* stop if return some t, else continue *)
  let ( let* ) x f = match x with Some t -> t | None -> f () in
  let if_ b c = if b then Some c else None in
  let* () = Option.map (fun x -> Error (x, [])) (Location.string2loc line) in
  let* () = if_ (line = "COQDEP VFILES") COQDEP in
  let* () = if_ (line = "CLEAN") CLEAN in
  let* () =
    if_
      (String.starts_with ~prefix:"coq_makefile" (String.trim line))
      (COQ_MAKEFILE line)
  in
  let* () =
    if_
      (String.starts_with ~prefix:"Time |" (String.trim line))
      (PRETTY_TABLE line)
  in
  let* () =
    if_ (String.starts_with ~prefix:"make" (String.trim line)) (Make line)
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
    (match l with
    | 0 -> ()
    | 1 ->
        ANSITerminal.printf [ ANSITerminal.Bold ] "Compiling ";
        ANSITerminal.printf [ ANSITerminal.blue ] "%s\n"
          (fst (List.hd state.building))
    | _ ->
        ANSITerminal.printf [ ANSITerminal.Bold ] "Compiling %d files:\n" l;
        List.iter
          (fun (s, _) ->
            ANSITerminal.printf [ ANSITerminal.Bold ] " - ";
            ANSITerminal.printf [ ANSITerminal.blue ] "%s\n" s)
          state.building);
    flush stdout;
    { state with printed = true })
  else state

let clear_current state =
  if state.printed then (
    (match List.length state.building with
    | 0 -> ()
    | 1 ->
        ANSITerminal.move_bol ();
        ANSITerminal.move_cursor 0 (-1);
        ANSITerminal.erase ANSITerminal.Below
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
          if is_error msg then (red, "Error", S_Error)
          else (magenta, "Warning", S_Warning)
        in
        printf [ Bold ] "%s\n" (Location.loc2string loc);
        Location.show_loc loc [ color ];
        printf [ Bold; color ] "%s" text;
        printf [] ":\n";
        Utils.print_error msg [ color ];
        printf [] "\n";
        let old_status = List.assoc_opt file state.building in
        {
          state with
          error = None;
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
  | COQ_MAKEFILE _line ->
      let state = resolve_error state in
      ANSITerminal.printf
        [ ANSITerminal.Bold; ANSITerminal.green ]
        "Creating makefile with coq_makefile\n";
      state
  | PRETTY_TABLE line ->
      let state = resolve_error state in
      ANSITerminal.printf [] "\n%s\n" line;
      state
  | Done d ->
      let state = resolve_error state in
      let file = Location.pretty_filename ~extension:".vo" d.file in
      let status = List.assoc_opt file state.building in
      let status = Option.value status ~default:S_Ok in
      Utils.print_time d.real;
      ANSITerminal.printf [] " | ";
      Utils.print_size d.mem;
      ANSITerminal.printf [] " | ";
      print_status status;
      ANSITerminal.printf [] " | ";
      Utils.print_file file;
      ANSITerminal.printf [] "\n";
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
  | End_of_file ->
      Unix.sleepf 0.5;
      main ic state
  | Sys.Break ->
      let state = clear_current state in
      let state = resolve_error state in
      ANSITerminal.printf [ ANSITerminal.yellow ] "INTERRUPTED\n";
      state

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
