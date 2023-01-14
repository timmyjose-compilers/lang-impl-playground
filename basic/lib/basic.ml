(* The absstract syntax *)

type unary_op = UMINUS | NOT

type binary_op =
  | PLUS
  | MINUS
  | MULT
  | DIV
  | MOD
  | EQ
  | NOTEQ
  | LESS
  | LESSEQ
  | GREATER
  | GREATEREQ
  | AND
  | OR

type expression =
  | ExpInt of int
  | ExpVar of string
  | ExpStr of string
  | ExpUnary of unary_op * expression
  | ExpBinary of expression * binary_op * expression

type command =
  | Rem of string
  | Input of string
  | Print of expression
  | Let of string * expression
  | Goto of int
  | If of expression * int

type line = { line_number : int; cmd : command }
type program = line list
type phrase = Line of line | Run | List | End

let priority_unary_op = function UMINUS -> 7 | NOT -> 1

let priority_binary_op = function
  | PLUS | MINUS -> 5
  | MULT | DIV -> 6
  | MOD -> 4
  | EQ | NOTEQ | LESS | LESSEQ | GREATER | GREATEREQ -> 3
  | AND | OR -> 2

(* printers *)

let pp_unary_op = function UMINUS -> "-" | NOT -> "!"

let pp_binary_op = function
  | PLUS -> "+"
  | MINUS -> "-"
  | MULT -> "*"
  | DIV -> "/"
  | MOD -> "%"
  | EQ -> " = "
  | NOTEQ -> " <> "
  | LESS -> " < "
  | LESSEQ -> " <= "
  | GREATER -> " > "
  | GREATEREQ -> " >= "
  | AND -> " & "
  | OR -> " | "

let parenthesis x = "(" ^ x ^ ")"

let pp_expression =
  let rec ppl pr = function
    | ExpInt n -> string_of_int n
    | ExpVar v -> v
    | ExpStr s -> s
    | ExpUnary (op, e) ->
        let res = pp_unary_op op ^ ppl (priority_unary_op op) e in
        if pr = 0 then res else parenthesis res
    | ExpBinary (e1, op, e2) ->
        let pr2 = priority_binary_op op in
        let res = ppl pr2 e1 ^ pp_binary_op op ^ ppr pr2 e2 in
        if pr2 >= pr then res else parenthesis res
  and ppr pr e =
    match e with
    | ExpBinary (e1, op, e2) ->
        let pr2 = priority_binary_op op in
        let res = ppl pr2 e1 ^ pp_binary_op op ^ ppr pr2 e2 in
        if pr2 > pr then res else parenthesis res
    | _ -> ppl pr e
  in
  ppl 0

let pp_command = function
  | Rem s -> "REM " ^ s
  | Goto n -> "GOTO " ^ string_of_int n
  | Let (v, e) -> "LET " ^ v ^ " = " ^ pp_expression e
  | Input v -> "INPUT " ^ v
  | Print e -> "PRINT " ^ pp_expression e
  | If (e, n) -> "IF " ^ pp_expression e ^ " THEN " ^ string_of_int n

let pp_line l = string_of_int l.line_number ^ " " ^ pp_command l.cmd

let read_to_string filename =
  try
    let infile = open_in filename in
    let len = in_channel_length infile in
    let buf = Buffer.create len in
    Buffer.add_channel buf infile len;
    close_in infile;
    Buffer.contents buf
  with Sys_error e -> failwith e

(* lexer *)

type lexeme =
  | Lint of int
  | Lident of string
  | Lstr of string
  | Lsym of string
  | Lend

let pp_lexeme = function
  | Lint n -> "Lint " ^ string_of_int n
  | Lident v -> "Lident " ^ v
  | Lstr s -> "Lstr " ^ s
  | Lsym sym -> "Lsym " ^ sym
  | Lend -> "Lend"

type lexer = { src : string; mutable pos : int; size : int }

exception Lexer_error

let init_lexer s = { src = s; pos = 0; size = String.length s }
let forward l = l.pos <- l.pos + 1
let forward_n l n = l.pos <- l.pos + n

let extract pred l =
  let s = l.src and p = l.pos in
  let rec extract_aux n =
    if n < l.size && pred s.[n] then extract_aux (n + 1) else n
  in
  let res = extract_aux p in
  l.pos <- res;
  String.sub l.src p (res - p)

let extract_int l =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  int_of_string (extract is_digit l)

let extract_ident l =
  let is_alphanum = function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
    | _ -> false
  in
  extract is_alphanum l

let rec lex l =
  let lex_char c =
    match c with
    | ' ' | '\t' ->
        forward l;
        lex l
    | 'a' .. 'z' | 'A' .. 'Z' -> Lident (extract_ident l)
    | '0' .. '9' -> Lint (extract_int l)
    | '"' ->
        forward l;
        let res = Lstr (extract (( <> ) '"') l) in
        forward l;
        res
    | '+' | '-' | '*' | '/' | '%' | '=' | '&' | '|' | '(' | ')' ->
        forward l;
        Lsym (String.make 1 c)
    | '<' | '>' -> (
        forward l;
        if l.pos >= l.size then Lsym (String.make 1 c)
        else
          let curr = l.src.[l.pos] in
          match (c, curr) with
          | '<', '=' ->
              forward l;
              Lsym "<="
          | '<', '>' ->
              forward l;
              Lsym "<>"
          | '>', '=' ->
              forward l;
              Lsym ">="
          | _ -> raise Lexer_error)
    | _ -> raise Lexer_error
  in

  if l.pos >= l.size then Lend else lex_char l.src.[l.pos]

(* parser *)

(** parsing expression using operator precedence parsing**)

(* type of elements for the pushdown stack *)
type exp_elem =
  | Texp of expression
  | Tunary of unary_op
  | Tbinary of binary_op
  | Tlp

exception Parser_error

let unary_sym = function "-" -> UMINUS | "!" -> NOT | _ -> raise Parser_error

let binary_sym = function
  | "+" -> PLUS
  | "-" -> MINUS
  | "*" -> MULT
  | "/" -> DIV
  | "%" -> MOD
  | "=" -> EQ
  | "<>" -> NOTEQ
  | "<" | "<=" -> LESSEQ
  | ">" -> GREATER
  | ">=" -> GREATEREQ
  | "&" -> AND
  | "|" -> OR
  | _ -> raise Parser_error

let tsym s =
  try Tbinary (binary_sym s) with Parser_error -> Tunary (unary_sym s)

let reduce pr = function
  | Texp e :: Tunary op :: st when priority_unary_op op >= pr ->
      Texp (ExpUnary (op, e)) :: st
  | Texp e1 :: Tbinary op :: Texp e2 :: st when priority_binary_op op >= pr ->
      Texp (ExpBinary (e2, op, e1)) :: st
  | _ -> raise Parser_error

let rec stack_or_reduce lexeme st =
  match (lexeme, st) with
  | Lint n, _ -> Texp (ExpInt n) :: st
  | Lident v, _ -> Texp (ExpVar v) :: st
  | Lstr s, _ -> Texp (ExpStr s) :: st
  | Lsym "(", _ -> Tlp :: st
  | Lsym ")", Texp e :: Tlp :: st -> Texp e :: st
  | Lsym ")", _ -> stack_or_reduce lexeme (reduce 0 st)
  | Lsym s, _ -> (
      let sym =
        if s <> "-" then tsym s
        else match st with Texp _ :: _ -> Tbinary MINUS | _ -> Tunary UMINUS
      in
      match sym with
      | Tunary op -> Tunary op :: st
      | Tbinary op -> (
          try stack_or_reduce lexeme (reduce (priority_binary_op op) st)
          with Parser_error -> Tbinary op :: st)
      | _ -> raise Parser_error)
  | _, _ -> raise Parser_error

let rec reduce_all = function
  | [] -> raise Parser_error
  | [ Texp e ] -> e
  | st -> reduce_all (reduce 0 st)

let parse_exp stop l =
  let p = ref 0 in
  let rec parse_one st =
    let ll =
      p := l.pos;
      lex l
    in
    if not (stop ll) then parse_one (stack_or_reduce ll st)
    else (
      l.pos <- !p;
      reduce_all st)
  in
  parse_one []

let parse_cmd l =
  match lex l with
  | Lident v -> (
      match v with
      | "REM" -> Rem (extract (fun _ -> true) l)
      | "GOTO" -> (
          match lex l with Lint n -> Goto n | _ -> raise Parser_error)
      | "INPUT" ->
          Input (match lex l with Lident v -> v | _ -> raise Parser_error)
      | "PRINT" -> Print (parse_exp (( = ) Lend) l)
      | "LET" -> (
          let l2 = lex l and l3 = lex l in
          match (l2, l3) with
          | Lident v, Lsym "=" -> Let (v, parse_exp (( = ) Lend) l)
          | _ -> raise Parser_error)
      | "IF" -> (
          let test = parse_exp (( = ) (Lident "THEN")) l in
          match
            ignore (lex l);
            lex l
          with
          | Lint n -> If (test, n)
          | _ -> raise Parser_error)
      | _ -> raise Parser_error)
  | _ -> raise Parser_error

let parse s =
  let l = init_lexer s in
  match lex l with
  | Lint n -> Line { line_number = n; cmd = parse_cmd l }
  | Lident "LIST" -> List
  | Lident "RUN" -> Run
  | Lident "END" -> End
  | _ -> raise Parser_error

(* evaluator *)

type value = Vint of int | Vstr of string | Vbool of bool
type environment = (string * value) list
type code = line array
type exec_state = { line : int; xprog : code; xenv : environment }

exception Run_error of int

let runerr n = raise (Run_error n)

exception Result_lookup_index of int

let lookup_index prog line =
  try
    for i = 0 to Array.length prog - 1 do
      let line_idx = prog.(i).line_number in
      if line_idx = line then raise (Result_lookup_index i)
      else if line_idx > line then raise (Result_lookup_index (-1))
    done;
    -1
  with Result_lookup_index idx -> idx

let assemble prog =
  let tprog = Array.of_list prog in
  for i = 0 to Array.length tprog - 1 do
    match tprog.(i).cmd with
    | Goto n ->
        let idx = lookup_index tprog n in
        tprog.(i) <- { (tprog.(i)) with cmd = Goto idx }
    | If (c, n) ->
        let idx = lookup_index tprog n in
        tprog.(i) <- { (tprog.(i)) with cmd = If (c, idx) }
    | _ -> ()
  done;
  tprog

let rec eval_exp n env exp =
  match exp with
  | ExpInt d -> Vint d
  | ExpVar v -> ( try List.assoc v env with Not_found -> runerr n)
  | ExpUnary (UMINUS, e) -> (
      match eval_exp n env e with Vint d -> Vint (-d) | _ -> runerr n)
  | ExpUnary (NOT, e) -> (
      match eval_exp n env e with Vbool b -> Vbool (not b) | _ -> runerr n)
  | ExpStr s -> Vstr s
  | ExpBinary (e1, op, e2) -> (
      match (eval_exp n env e1, op, eval_exp n env e2) with
      | Vint d1, PLUS, Vint d2 -> Vint (d1 + d2)
      | Vint d1, MINUS, Vint d2 -> Vint (d1 - d2)
      | Vint d1, MULT, Vint d2 -> Vint (d1 * d2)
      | Vint d1, DIV, Vint d2 when d2 <> 0 -> Vint (d1 / d2)
      | Vint d1, MOD, Vint d2 when d2 <> 0 -> Vint (d1 mod d2)
      | Vint d1, EQ, Vint d2 -> Vbool (d1 = d2)
      | Vint d1, NOTEQ, Vint d2 -> Vbool (d1 <> d2)
      | Vint d1, LESS, Vint d2 -> Vbool (d1 < d2)
      | Vint d1, LESSEQ, Vint d2 -> Vbool (d1 <= d2)
      | Vint d1, GREATER, Vint d2 -> Vbool (d1 > d2)
      | Vint d1, GREATEREQ, Vint d2 -> Vbool (d1 >= d2)
      | Vbool b1, AND, Vbool b2 -> Vbool (b1 && b2)
      | Vbool b1, OR, Vbool b2 -> Vbool (b1 || b2)
      | Vstr s1, PLUS, Vstr s2 -> Vstr (s1 ^ s2)
      | _, _, _ -> runerr n)

let rec add v e env =
  match env with
  | [] -> [ (v, e) ]
  | (w, f) :: l -> if w = v then (v, e) :: l else (w, f) :: add v e l

let print_value = function
  | Vint n -> print_int n
  | Vbool true -> print_string "true"
  | Vbool false -> print_string "false"
  | Vstr s -> print_string s

let next_line state =
  let n = state.line + 1 in
  if n < Array.length state.xprog then n else -1

let eval_cmd state =
  match state.xprog.(state.line).cmd with
  | Rem _ -> { state with line = next_line state }
  | Print e ->
      print_value (eval_exp state.line state.xenv e);
      print_newline ();
      { state with line = next_line state }
  | Let (v, e) ->
      let ev = eval_exp state.line state.xenv e in
      { state with line = next_line state; xenv = add v ev state.xenv }
  | Goto n -> { state with line = n }
  | Input v ->
      let d = try read_int () with Failure _ -> 0 in
      { state with line = next_line state; xenv = add v (Vint d) state.xenv }
  | If (c, n) -> (
      match eval_exp state.line state.xenv c with
      | Vbool true -> { state with line = n }
      | Vbool false -> { state with line = next_line state }
      | _ -> runerr n)

let rec run state = if state.line = -1 then state else run (eval_cmd state)
