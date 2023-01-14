open Basic

let rec insert line p =
  match p with
  | [] -> [ line ]
  | l :: prog ->
      if l.line_number < line.line_number then l :: insert line prog
      else line :: l :: prog

let print_prog prog =
  let print_line x =
    print_string (pp_line x);
    print_newline ()
  in
  print_newline ();
  List.iter print_line prog;
  print_newline ()

type loop_state = { prog : program; env : environment }

exception End_error

let one_command state =
  print_string "> ";
  flush stdout;
  try
    match parse (input_line stdin) with
    | Line l -> { state with prog = insert l state.prog }
    | List ->
        print_prog state.prog;
        state
    | Run ->
        let tprog = assemble state.prog in
        let xstate = run { line = 0; xprog = tprog; xenv = state.env } in
        { state with env = xstate.xenv }
    | End -> raise End_error
  with
  | Lexer_error ->
      print_string "Illegal character\n";
      state
  | Parser_error ->
      print_string "syntax error\n";
      state
  | Run_error n ->
      print_string "runtime error at line ";
      print_int n;
      print_newline ();
      state

let go () =
  try
    print_string "Mini-BASIC v0.1\n\n";
    let rec loop state = loop (one_command state) in
    loop { prog = []; env = [] }
  with End_error -> print_string "Bye!\n\n" ;;

go ()
