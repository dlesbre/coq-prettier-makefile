type status =
  | S_Ok
  | S_Error
  | S_Warning
  | S_Compiling
  | S_Testing
  | S_TestOk
  | S_TestFail

let print_status = function
  | S_Ok -> ANSITerminal.printf [ ANSITerminal.green ] "DONE       "
  | S_TestOk -> ANSITerminal.printf [ ANSITerminal.green ] "TEST OK    "
  | S_Error -> ANSITerminal.printf [ ANSITerminal.red ] "ERROR      "
  | S_TestFail -> ANSITerminal.printf [ ANSITerminal.red ] "TEST FAIL  "
  | S_Warning -> ANSITerminal.printf [ ANSITerminal.magenta ] "WARNING    "
  | S_Compiling -> ANSITerminal.printf [] "COMPILING  "
  | S_Testing -> ANSITerminal.printf [] "TESTING    "

let max_status l r =
  match (l, r) with
  | S_Error, _ | _, S_Error -> S_Error
  | S_TestFail, _ | _, S_TestFail -> S_TestFail
  | S_Warning, _ | _, S_Warning -> S_Warning
  | S_Ok, _ | _, S_Ok -> S_Ok
  | S_TestOk, _ | _, S_TestOk -> S_TestOk
  | S_Compiling, _ | _, S_Compiling -> S_Compiling
  | S_Testing, S_Testing -> S_Testing

let status_done = function
  | S_Compiling -> S_Ok
  | S_Testing -> S_TestOk
  | s -> s

let pretty_time t =
  let t' = abs (int_of_float t) in
  let hours = t' / 3600 in
  let t' = t' mod 3600 in
  Format.sprintf "%02d:%02d:%02d" hours (t' / 60) (t' mod 60)

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

let pretty_size ?(padding = 5) value =
  let base = 1000. in
  let rec aux value suffix =
    if value < base then Format.sprintf "%*.1f " padding value ^ List.hd suffix
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
      if Future.string_starts_with ~prefix str then
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
    "---------|----------|-------------|-----------------------------------------------\n"

let spaces = [ ' '; '\n'; '\t' ]

let str2argv str =
  let len = String.length str in
  let rec parse_start i l =
    if i = len then l
    else if List.mem str.[i] spaces then parse_start (i + 1) l
    else if List.mem str.[i] [ '\''; '\"' ] then
      parse_quoted str.[i] (i + 1) (i + 1) l
    else parse_word i (i + 1) l
  and parse_word start i l =
    if i = len then String.sub str start (i - start) :: l
    else if List.mem str.[i] spaces then
      parse_start (i + 1) (String.sub str start (i - start) :: l)
    else if i + 1 < len && str.[i] = '\\' then parse_word start (i + 2) l
    else parse_word start (i + 1) l
  and parse_quoted quote start i l =
    if i = len then String.sub str start (i - start) :: l
    else if str.[i] = quote then
      parse_start (i + 1) (String.sub str start (i - start) :: l)
    else if str.[i] = '\\' then parse_quoted quote start (i + 2) l
    else parse_quoted quote start (i + 1) l
  in
  List.rev (parse_start 0 [])
