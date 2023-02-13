module SMap = Map.Make (String)

let str_end = "coq-prettier-makefile-done"
let update_time = 0.05

let rec fetch_input ic =
  try
    if not !Lines.is_done then
      let line = input_line ic in
      if line = str_end then Lines.is_done := true
      else (
        Queue.add line Lines.todo;
        fetch_input ic)
  with
  | End_of_file ->
      Unix.sleepf update_time;
      fetch_input ic
  | Sys.Break | Sys_error _ -> Thread.exit ()

let rec mainloop state start =
  let open Lines in
  try
    if Queue.is_empty todo then (
      let state = clear_current state in
      if !is_done then print_final true (Unix.time () -. start) state
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
      print_final false (Unix.time () -. start) state

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
      let partial = String.concat Filename.dir_sep (f :: fs) in
      let smap = smap_safe_add partial target smap in
      let smap =
        smap_safe_add (Filename.remove_extension partial) target smap
      in
      build_path target smap fs

let add_v_file file smap =
  if Filename.check_suffix file ".v" then
    let target = file ^ "o" in
    build_path target smap (Lines.split_path target)
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
      if
        Lines.is_prefix "-" arg
        || List.mem arg coq_makefile_targets
        || String.contains arg '='
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
  if targets = [] then argv
  else Format.sprintf "TGTS=\"%s\"" (String.concat " " targets) :: argv

let main () =
  Sys.catch_break true;
  let argv = get_argv () in
  let in_c, out_c, err_c =
    Unix.open_process_full
      (Filename.quote_command "make" argv ^ " 2>&1; echo " ^ str_end)
      (Unix.environment ())
  in
  let _ = Thread.create fetch_input in_c in
  let _ = mainloop Lines.initial_state (Unix.time ()) in
  (* Rings terminal bell *)
  ANSITerminal.printf [] "\007";
  Unix.close_process_full (in_c, out_c, err_c)

let _ = main ()
