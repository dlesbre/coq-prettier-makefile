type t = {
  file : string;
  line_start : int;
  line_end : int;
  char_start : int;
  char_end : int;
}

let pp_pair fmt (a, b) =
  if a = b then Format.fprintf fmt " %d" a else Format.fprintf fmt "s %d-%d" a b

let ( let* ) = Option.bind

let read_pair line =
  match String.split_on_char '-' line with
  | [ s ] ->
      let* i = int_of_string_opt s in
      Some (i, i)
  | [ s; e ] ->
      let* s = int_of_string_opt s in
      let* e = int_of_string_opt e in
      Some (s, e)
  | _ -> None

(** Remove leading "./" and extension if supplied *)
let pretty_filename ?(extension = []) filename =
  let len = String.length filename in
  let filename =
    if Future.string_starts_with ~prefix:"./" filename then
      String.sub filename 2 (len - 2)
    else filename
  in
  let rec rm_extension = function
    | [] -> filename
    | suffix :: suffixes ->
        let len_suffix = String.length suffix in
        if Future.string_ends_with ~suffix filename then
          String.sub filename 0 (len - len_suffix)
        else rm_extension suffixes
  in
  rm_extension extension

let string2loc line =
  try
    Scanf.sscanf line "File %S, line%s %s@, character%s %s@:"
      (fun file _ lines _ chars ->
        let* line_start, line_end = read_pair lines in
        let* char_start, char_end = read_pair chars in
        Some
          {
            file = pretty_filename file;
            line_start;
            line_end;
            char_start;
            char_end;
          })
  with Scanf.Scan_failure _ -> None

let loc2string loc =
  Format.asprintf "File \"%s\", line%a, character%a" loc.file pp_pair
    (loc.line_start, loc.line_end)
    pp_pair
    (loc.char_start, loc.char_end)

(** Number of digits in line number, used to properly align *)
let digits loc =
  max
    (String.length (string_of_int loc.line_start))
    (String.length (string_of_int loc.line_end))

let pointers loc line_nb line_length =
  String.init line_length (fun i ->
      let i = i + 1 in
      (* start index at 1 *)
      let is_pointed =
        if line_nb = loc.line_start && line_nb = loc.line_end then
          i >= loc.char_start && i <= loc.char_end
        else if line_nb = loc.line_start then i >= loc.char_start
        else if line_nb = loc.line_end then i <= loc.char_end
        else true
      in
      if is_pointed then '^' else ' ')

let show_loc loc style =
  let digits = digits loc in
  try
    let ic = open_in loc.file in
    for line_nb = 1 to loc.line_end do
      let line = input_line ic in
      (* replace tabs as they mess alignements *)
      let line = Str.global_replace (Str.regexp "\t") " " line in
      if line_nb >= loc.line_start then (
        ANSITerminal.printf [] "%*d | %s\n %*s| " digits line_nb line digits "";
        ANSITerminal.printf style "%s\n"
          (pointers loc line_nb (String.length line)))
    done;
    close_in ic
  with _ -> ()

let get_file loc = loc.file
