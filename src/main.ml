type file = string

module LS_Set = Set.Make (struct
  type t = Location.t * string list

  let compare = compare
end)

type state = {
  building : (file * int) list;
  error : (Location.t * string list) option;
  seen : LS_Set.t;
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
  | Unknown of string

let ( let* ) x f = match x with Some t -> t | None -> f ()

let parse_line line =
  let* () = Option.map (fun x -> Error (x, [])) (Location.string2loc line) in
  let* () = if line = "COQDEP VFILES" then Some COQDEP else None in
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
  let l = List.length state.building in
  ANSITerminal.printf [ ANSITerminal.Bold ] "Compiling %d files:\n" l;
  List.iter
    (fun (s, _) ->
      ANSITerminal.printf [ ANSITerminal.Bold ] " - ";
      ANSITerminal.printf [ ANSITerminal.blue ] "%s\n" s)
    state.building

let clear_current state =
  ANSITerminal.move_bol ();
  ANSITerminal.move_cursor 0 (-List.length state.building - 1);
  ANSITerminal.erase ANSITerminal.Below

let pretty_time t =
  if t < 60.0 then Format.sprintf "%.1f s" t
  else if t < 3600. then
    let minutes = t /. 60. in
    Format.sprintf "%0f m %1f s" minutes (t -. (60. *. minutes))
  else
    let hours = t /. 3600. in
    let minutes = (t /. 60.) -. (60. *. hours) in
    Format.sprintf "%.0f h %.0f m %.1f s" hours minutes
      (t -. (60. *. minutes) -. (3600. *. hours))

let pretty_size value =
  let base = 1000. in
  let rec aux value suffix =
    if value < base then Format.sprintf "%.2f " value ^ List.hd suffix
    else aux (value /. 1000.) (List.tl suffix)
  in
  aux (float_of_int value) [ "ko"; "Mo"; "Go"; "To"; "Po"; "Eo"; "Zo" ]

let resolve_error state =
  match state.error with
  | Some (loc, msg) ->
      if LS_Set.mem (loc, msg) state.seen then state
      else
        let state = { state with seen = LS_Set.add (loc, msg) state.seen } in
        let file =
          Location.pretty_filename ~extension:".v" (Location.get_file loc)
        in
        let msg = List.fold_left (fun acc m -> acc ^ m ^ "\n") "" msg in
        if String.starts_with ~prefix:"Error:" msg then (
          ANSITerminal.printf [] "%s: " file;
          ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.red ] "ERROR\n";
          ANSITerminal.printf [ ANSITerminal.Bold ] "%s\n"
            (Location.loc2string loc);
          Location.show_loc loc [ ANSITerminal.red ];
          ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.red ] "Error";
          ANSITerminal.printf [] "%s" (String.sub msg 5 (String.length msg - 5));
          { state with building = List.remove_assoc file state.building })
        else (
          ANSITerminal.printf [ ANSITerminal.Bold ] "%s\n"
            (Location.loc2string loc);
          Location.show_loc loc [ ANSITerminal.magenta ];
          if String.starts_with ~prefix:"Warning" msg then (
            ANSITerminal.printf
              [ ANSITerminal.Bold; ANSITerminal.magenta ]
              "Warning";
            ANSITerminal.printf [] "%s"
              (String.sub msg 7 (String.length msg - 7)))
          else ANSITerminal.printf [] "%s" msg;
          { state with building = List.remove_assoc file state.building })
  | None -> state

let print_line state = function
  | COQC file ->
      let state = resolve_error state in
      let file = Location.pretty_filename ~extension:".v" file in
      let state = { state with building = state.building @ [ (file, 0) ] } in
      state
  | COQDEP ->
      let state = resolve_error state in
      ANSITerminal.printf
        [ ANSITerminal.Bold; ANSITerminal.green ]
        "Finding dependencies with COQDEP\n";
      state
  | Make m ->
      let state = resolve_error state in
      ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.yellow ] "make";
      ANSITerminal.printf [] "%s\n" (String.sub m 4 (String.length m - 4));
      state
  | Done d ->
      let state = resolve_error state in
      let file = Location.pretty_filename ~extension:".vo" d.file in
      ANSITerminal.printf [] "%s: " file;
      ANSITerminal.printf [ ANSITerminal.Bold; ANSITerminal.green ] "DONE";
      ANSITerminal.printf [] " in %s %s\n" (pretty_time d.real)
        (pretty_size d.mem);
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

let do_line state line =
  clear_current state;
  let state = print_line state line in
  print_current state;
  state

let rec main state =
  try
    let line = read_line () in
    main (do_line state (parse_line line))
  with End_of_file -> resolve_error state

let _ =
  let state = { building = []; seen = LS_Set.empty; error = None } in
  print_current state;
  main state
