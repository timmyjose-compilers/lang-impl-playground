type lexeme = Lint of int | Lsym of string | Lend
type lexer = { src : string; mutable pos : int; size : int }

exception Lexer_error of string

let init_lexer s = { src = s; pos = 0; size = String.length s }
let forward l = l.pos <- l.pos + 1
let forward_n l n = l.pos <- l.pos + n

let extract pred l =
  let st = l.src and pos = l.pos in
  let rec extract_aux n =
    if n < l.size && pred st.[n] then extract_aux (n + 1) else n
  in
  let res = extract_aux pos in
  l.pos <- res;
  String.sub l.src pos (res - pos)

let extract_int l =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  int_of_string (extract is_digit l)

let rec lex l =
  let lex_char c =
    match c with
    | ' ' | '\n' | '\t' ->
        forward l;
        lex l
    | '0' .. '9' -> Lint (extract_int l)
    | '+' | '-' | '*' | '/' | '%' | '(' | ')' ->
        forward l;
        Lsym (String.make 1 c)
    | _ -> raise (Lexer_error ("Invalid character: " ^ String.make 1 c))
  in
  if l.pos >= l.size then Lend else lex_char l.src.[l.pos]
