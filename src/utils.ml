type status = S_Ok | S_Error | S_Warning | S_Compiling

let print_status = function
  | S_Ok -> ANSITerminal.printf [ ANSITerminal.green ] "DONE     "
  | S_Error -> ANSITerminal.printf [ ANSITerminal.red ] "ERROR    "
  | S_Warning -> ANSITerminal.printf [ ANSITerminal.magenta ] "WARNING  "
  | S_Compiling -> ANSITerminal.printf [] "COMPILING"

let max_status l r =
  match (l, r) with
  | S_Error, _ | _, S_Error -> S_Error
  | S_Warning, _ | _, S_Warning -> S_Warning
  | S_Ok, _ | _, S_Ok -> S_Ok
  | S_Compiling, S_Compiling -> S_Compiling

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
    if value < base then Format.sprintf "%5.1f " value ^ List.hd suffix
    else aux (value /. 1000.) (List.tl suffix)
  in
  aux (float_of_int value) [ "ko"; "Mo"; "Go"; "To"; "Po"; "Eo"; "Zo" ]

let size_breaks =
  [
    (100_000, ANSITerminal.green);
    (250_000, ANSITerminal.cyan);
    (500_000, ANSITerminal.blue);
    (750_000, ANSITerminal.yellow);
    (1_000_000, ANSITerminal.magenta);
  ]

let print_size size =
  match size with
  | None -> ANSITerminal.printf [] "  ....  "
  | Some size ->
      let rec aux = function
        | [] -> ANSITerminal.red
        | (s, c) :: q -> if size <= s then c else aux q
      in
      ANSITerminal.printf [ aux size_breaks ] "%s" (pretty_size size)

let print_file file =
  let path = String.split_on_char '/' file in
  let rec aux = function
    | [ t ] -> ANSITerminal.printf [] "%s" t
    | t :: q ->
        ANSITerminal.printf [] "%s" t;
        ANSITerminal.printf [ ANSITerminal.blue ] "/";
        aux q
    | [] -> ()
  in
  aux path

let rec strip_prefix str = function
  | [] -> str
  | prefix :: ps ->
      if String.starts_with ~prefix str then
        Str.string_after str (String.length prefix)
      else strip_prefix str ps

let print_error str format =
  let str = String.trim str in
  let str =
    strip_prefix str
      [
        "ERROR:";
        "Error:";
        "error:";
        "E:";
        "ERROR";
        "Error";
        "error";
        "WARNING:";
        "Warning:";
        "warning:";
        "W:";
        "WARNING";
        "Warning";
        "warning";
      ]
  in
  let str = String.trim str in
  List.iter
    (fun line ->
      ANSITerminal.printf format "  | ";
      ANSITerminal.printf [] "%s\n" line)
    (String.split_on_char '\n' str)

let print_file_line filename status time mem =
  print_time time;
  ANSITerminal.printf [] " | ";
  print_size mem;
  ANSITerminal.printf [] " | ";
  print_status status;
  ANSITerminal.printf [] " | ";
  print_file filename;
  ANSITerminal.printf [] "\n"

let print_separator () =
  ANSITerminal.printf [ ANSITerminal.Bold ]
    "---------|----------|-----------|-----------------------------------------------\n"
