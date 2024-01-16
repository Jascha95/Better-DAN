{
type token =
    PLUS
  | MINUS
  | STAR
  | SLASH
  | IDENT of string
  | INT of int
  | FLOAT of float
  | LET
  | BE
  | IN
  | LPAREN
  | RPAREN
  | EOF

let string_of_token = function
    PLUS    -> "PLUS"
  | MINUS   -> "MINUS"
  | STAR    -> "STAR"
  | SLASH   -> "SLASH"
  | IDENT s -> "IDENT(" ^ s ^ ")"
  | INT i   -> "INT(" ^ string_of_int i ^ ")"
  | FLOAT f -> "FLOAT(" ^ string_of_float f ^ ")"
  | LET     -> "LET"
  | BE      -> "BE"
  | IN      -> "IN"
  | LPAREN  -> "LPAREN"
  | RPAREN  -> "RPAREN"
  | EOF     -> "EOF"
}

let dec_digit = ['0' - '9']
let hex_digit = dec_digit | ['A' - 'F' 'a' - 'f']
let sign = '-' | ""

let dec_int_lit = sign dec_digit+

let hex_prefix = "0x" | "0X"
let hex_int_lit = sign hex_prefix hex_digit+

let int_lit = dec_int_lit | hex_int_lit
let float_lit = sign dec_digit+ '.' dec_digit+

let letter = ['a' - 'z' 'A' - 'Z']
let ident_start = letter | '_'
let ident_rest = ident_start | dec_digit
let ident = ident_start ident_rest*

rule token = parse
    [' ' '\t' '\r' '\n']+   { token lexbuf }
  | '+'                     { PLUS }
  | '-'                     { MINUS }
  | '*'                     { STAR }
  | '/'                     { SLASH }
  | '('                     { LPAREN }
  | ')'                     { RPAREN }
  | int_lit as lxm          { INT (int_of_string lxm) }
  | float_lit as lxm        { FLOAT (float_of_string lxm) }
  | "let"                   { LET }
  | "be"                    { BE }
  | "in"                    { IN }
  | ident as lxm            { IDENT lxm }
  | eof                     { EOF }

{

let _ =
  let fname = Sys.argv.(1) in
  let chan = open_in fname in
  let buf = Lexing.from_channel chan in
  let rec lex () =
    match token buf with
        EOF -> ()
      | t   -> print_endline (string_of_token t); lex ()
  in lex ()
}
