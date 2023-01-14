open Arith_evaluator.Lexer
open Arith_evaluator.Parser
open Arith_evaluator.Evaluator

let go () =
  print_endline "Welcome to the arithmetic expression evaluator";
  let rec loop () =
    print_string ">> ";
    flush stdout;
    try
      print_int (eval_exp (parse_exp (( = ) Lend) (init_lexer (read_line ()))));
      print_newline ();
      loop ()
    with
    | Lexer_error lex_err ->
        print_endline lex_err;
        loop ()
    | Parser_error parse_err ->
        print_endline parse_err;
        loop ()
    | Eval_error eval_err ->
        print_endline eval_err;
        loop ()
    | _ -> ()
  in
  loop ()
;;

go ()
