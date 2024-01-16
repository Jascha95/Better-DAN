{
  open Parser
}


(* Anything but star *)
let not_star           =
  [^ '*']  

(* Anything but star and slash *)
let not_star_not_slash =
  [^ '*' '/'] 

(* comment *)
let traditional_comment =
  "/*" not_star+ "*"+ (not_star_not_slash not_star* "*"+)* '/'

let blanks =
  [' ' '\t' '\n']+

let ident =
  ['A'-'Z' 'a'-'z'] ['A'-'Z' 'a'-'z' '0'-'9']*

let i = ['0'-'9']+
let lambda = "\\" 
  | ("L"|"l") ("A" | "a") ("M" | "m") ("D" | "d") ("A" | "a") 
  | ("F" | "f") ("U" | "u") ("N" | "n")

rule token = parse
  | "let"                 { LET }
  | "in"                  { IN }
  | "new_"                { NEWL }
  | "new"                 { NEW }
  | "undef" | "undefined" { UNDEF }
  | "M_"                  { MASK }
  | "."                   { DOT }
  | ":="                  { COLONEQ }
  | "="                   { EQ  }
  | "("                   { LPARAN }
  | ")"                   { RPARAN }
  | ","                   { COMMA  }
  | lambda                { LAMBDA }
  | "]"                   { RBRACKET }
  | "["                   { LBRACKET }
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | "l" (i as i)          { LOC (Location.Loc.from_int (int_of_string i)) }
  | ident as i            { IDENT i }
  | i as i                { INT (int_of_string i)} 
  | blanks                { token lexbuf  }
  | traditional_comment   { token lexbuf  } 
  | eof                   { LEof }

