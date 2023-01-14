open Ast

exception Eval_error of string

let rec eval_exp = function
  | ExpInt n -> n
  | ExpUnary (UMINUS, e) -> -eval_exp e
  | ExpUnary (UPLUS, e) -> eval_exp e
  | ExpBinary (e1, op, e2) -> (
      match op with
      | PLUS -> eval_exp e1 + eval_exp e2
      | MINUS -> eval_exp e1 - eval_exp e2
      | MULT -> eval_exp e1 * eval_exp e2
      | DIV -> (
          match eval_exp e2 with
          | 0 -> raise (Eval_error "cannot divide by 0")
          | e2res -> eval_exp e1 / e2res)
      | MOD -> eval_exp e1 mod eval_exp e2)
