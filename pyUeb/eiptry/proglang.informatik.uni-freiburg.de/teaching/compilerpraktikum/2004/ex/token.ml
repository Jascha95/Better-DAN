
type token =
  | INT of int
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAREN
  | RPAREN
  | EOL

let string_of_token t =
  match t with 
    INT i  -> "INT(" ^ string_of_int i ^ ")"
  | PLUS   -> "PLUS"
  | MINUS  -> "MINUS"
  | TIMES  -> "TIMES"
  | DIV    -> "DIV"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EOL    -> "EOL"
