(* File calc.ml *)

open Token

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let next_token = Lexer.token lexbuf in
        print_string (string_of_token next_token);
        print_newline ();
        flush stdout
    done
  with Lexer.Eof ->
    exit 0
