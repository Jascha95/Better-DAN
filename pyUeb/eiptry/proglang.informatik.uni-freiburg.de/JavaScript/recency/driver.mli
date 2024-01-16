open Gen

type p = {
  e: Syntax.e;
  se: string;
  sc: string;
  r: Gen.result;
  c: Gen.changed;
}

val token_to_string : Parser.token -> string
val parse : (Syntax.e -> string -> 'a) -> Lexing.lexbuf -> 'a
val gen : (p -> 'a) -> Lexing.lexbuf -> 'a
val solve : (p -> 'a) -> Lexing.lexbuf -> 'a
