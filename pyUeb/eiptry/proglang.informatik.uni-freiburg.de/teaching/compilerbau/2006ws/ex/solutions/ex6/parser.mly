%token OR STAR LPAREN RPAREN
%token <char> SYMBOL
%token EOF

%start re
%type <Re.re> re

%%

re: re OR re_1 { Re.Union($1, $3) }
  | re_1       { $1 }
  ;

re_1: re_1 re_2 { Re.Concat($1, $2) }
    | re_2      { $1 }
    ;

re_2: re_3 STAR { Re.Repeat($1) }
    | re_3      { $1 }
    ;

re_3: SYMBOL           { Re.Symbol($1) }
    | LPAREN re RPAREN { $2 }
    ;
