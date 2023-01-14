type unary_op = UMINUS | UPLUS
type binary_op = PLUS | MINUS | MULT | DIV | MOD

type expression =
  | ExpInt of int
  | ExpUnary of unary_op * expression
  | ExpBinary of expression * binary_op * expression

let priority_uop = function UMINUS | UPLUS -> 4
let priority_bop = function MOD -> 1 | PLUS | MINUS -> 2 | MULT | DIV -> 3

(* pretty printing *)

let pp_uop = function UMINUS -> "-" | UPLUS -> "+"

let pp_binop = function
  | PLUS -> "+"
  | MINUS -> "-"
  | MULT -> "*"
  | DIV -> "/"
  | MOD -> "%"

let parenthesise x = "(" ^ x ^ ")"

let pp_expression =
  let rec ppl pr = function
    | ExpInt n -> string_of_int n
    | ExpUnary (op, e) ->
        let res = pp_uop op ^ ppl (priority_uop op) e in
        if pr = 0 then res else parenthesise res
    | ExpBinary (e1, op, e2) ->
        let pr2 = priority_bop op in
        let res = ppl pr2 e1 ^ pp_binop op ^ ppr pr2 e2 in
        if pr2 >= pr then res else parenthesise res
  and ppr pr e =
    match e with
    | ExpBinary (e1, op, e2) ->
        let pr2 = priority_bop op in
        let res = ppl pr2 e1 ^ pp_binop op ^ ppr pr2 e2 in
        if pr2 > pr then res else parenthesise res
    | _ -> ppl pr e
  in
  ppl 0
