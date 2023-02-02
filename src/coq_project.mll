{

type token = Arg of string | Eof

let str char = String.init 1 (fun _ -> char)

let acc = ref ""
}

(* ======== § Grammar from https://coq.inria.fr/refman/practical-tools/utilities.html#computing-module-dependencies ======== *)
let blank = [' ' '\t' '\n']

(* let comment_char = [^ '\n'] *)
let quoted_char = [^ '"']
(* let unquoted_char = [^ '#'] # blank
let string_start_char = [^ '#' '"'] # blank *)

rule token = parse
  | blank+  { token lexbuf }
  | '#'     { if comment lexbuf then Eof else token lexbuf }
  | '"'     { quoted lexbuf }
  | _ as c  { acc := str c; unquoted lexbuf }
  | eof     { Eof }

and comment = parse
  | eof     { true }
  | '\n'    { false }
  | _       { comment lexbuf }

and quoted = parse
  | '"'     { let s = !acc in acc := ""; Arg s }
  | eof     { Eof }
  | _ as c  { acc := !acc^str c; quoted lexbuf }

and unquoted = parse
  | blank   { let s = !acc in acc := ""; Arg s }
  | '#'     { let s = !acc in acc := ""; let _ = comment lexbuf in Arg s }
  | _ as c  { acc := !acc^str c; unquoted lexbuf }
