(** Reimplementation of [String.starts_with] for older OCaml compilers *)
let string_starts_with ~prefix str =
  let len_prefix = String.length prefix in
  if len_prefix > String.length str then false
  else
    let rec runner i =
      i = len_prefix || (Char.equal prefix.[i] str.[i] && runner (i + 1))
    in
    runner 0

(** Reimplementation of [String.ends_with] for older OCaml compilers *)
let string_ends_with ~suffix str =
  let len_suffix = String.length suffix in
  let len_str = String.length str in
  if len_suffix > len_str then false
  else
    let diff = len_str - len_suffix in
    let rec runner i =
      i = len_suffix || (Char.equal suffix.[i] str.[i + diff] && runner (i + 1))
    in
    runner 0
