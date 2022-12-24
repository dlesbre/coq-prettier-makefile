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
