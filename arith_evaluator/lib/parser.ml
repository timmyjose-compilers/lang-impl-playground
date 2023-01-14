open Ast
open Lexer

type st_elem = Texp of expression | Tunr of unary_op | Tbin of binary_op | Tlp

exception Parser_error of string

let uop_of_sym s =
  match s with
  | "-" -> UMINUS
  | "+" -> UPLUS
  | _ -> raise (Parser_error ("Invalid unary operator symbol: " ^ s))

let bop_of_sym s =
  match s with
  | "+" -> PLUS
  | "-" -> MINUS
  | "*" -> MULT
  | "/" -> DIV
  | "%" -> MOD
  | _ -> raise (Parser_error ("Invalid binary operator symbol: " ^ s))

let tsym s =
  try Tbin (bop_of_sym s) with Parser_error _ -> Tunr (uop_of_sym s)

let reduce pr = function
  | Texp e :: Tunr op :: st when priority_uop op >= pr ->
      Texp (ExpUnary (op, e)) :: st
  | Texp e1 :: Tbin op :: Texp e2 :: st when priority_bop op >= pr ->
      Texp (ExpBinary (e2, op, e1)) :: st
  | _ -> raise (Parser_error "Could not reduce stack")

let rec stack_or_reduce lexeme stack =
  match (lexeme, stack) with
  | Lint n, _ -> Texp (ExpInt n) :: stack
  | Lsym "(", _ -> Tlp :: stack
  | Lsym ")", Texp e :: Tlp :: st -> Texp e :: st
  | Lsym ")", _ -> stack_or_reduce lexeme (reduce 0 stack)
  | Lsym s, _ -> (
      let sym =
        match s with
        | "+" -> (
            match stack with Texp _ :: _ -> Tbin PLUS | _ -> Tunr UPLUS)
        | "-" -> (
            match stack with Texp _ :: _ -> Tbin MINUS | _ -> Tunr UMINUS)
        | _ -> tsym s
      in
      match sym with
      | Tunr op -> Tunr op :: stack
      | Tbin op -> (
          try stack_or_reduce lexeme (reduce (priority_bop op) stack)
          with Parser_error _ -> Tbin op :: stack)
      | _ -> raise (Parser_error "Invalid state"))
  | _, _ -> raise (Parser_error "Invalid state")

let rec reduce_all = function
  | [] -> raise (Parser_error "Invalid expression")
  | [ Texp ast ] -> ast
  | st -> reduce_all (reduce 0 st)

let parse_exp stop l =
  let p = ref 0 in
  let rec parse_one stack =
    let cl =
      p := l.pos;
      lex l
    in
    if not (stop cl) then parse_one (stack_or_reduce cl stack)
    else (
      l.pos <- !p;
      reduce_all stack)
  in
  parse_one []
