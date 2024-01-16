exception Parse_error

open Arith
open Lexer

let rec parse_S = function
    LET :: IDENT x :: BE :: rest ->
      let (e1, rest') = parse_S rest in
        begin
          match rest' with
              IN :: rest'' ->
                let (e2, rest''') = parse_S rest'' in
                  (Let (x, e1, e2), rest''')
            | _ -> raise Parse_error
        end
  | tokens -> parse_T tokens

and parse_T tokens = 
  let (e, rest) = parse_E tokens in
    parse_T' e rest

and parse_T' e1 = function
    PLUS :: rest ->
      let (e2, rest') = parse_E rest in
        parse_T' (Binop (e1, Add, e2)) rest'
  | MINUS :: rest ->
      let (e2, rest') = parse_E rest in
        parse_T' (Binop (e1, Sub, e2)) rest'
  | tokens -> (e1, tokens)

and parse_E tokens =
  let (e, rest) = parse_F tokens in
    parse_E' e rest

and parse_E' e1 = function
    STAR :: rest ->
      let (e2, rest') = parse_F rest in
        parse_E' (Binop (e1, Mul, e2)) rest'
  | SLASH :: rest ->
      let (e2, rest') = parse_F rest in
        parse_E' (Binop (e1, Div, e2)) rest'
  | tokens -> (e1, tokens)

and parse_F = function
    INT i :: rest -> (Number i, rest)
  | IDENT s :: rest -> (Var s, rest)
  | LPAREN :: rest ->
      begin
        match parse_S rest with
            (e, RPAREN :: rest') -> (e, rest')
          | _ -> raise Parse_error
      end
  | _ -> raise Parse_error
        

let parse = parse_S


      
  
