%token LET
%token IN
%token NEW
%token NEWL
%token UNDEF

%token MASK
%token DOT
%token EQ
%token COLONEQ
%token LPARAN
%token RPARAN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token <Location.Loc.t> LOC
%token COMMA
%token LAMBDA

%token <string> IDENT
%token <int> INT

%token LEof
 
 
%start prog
%type <Syntax.e> prog

%%
prog :
  e                                              { $1 }
| e  LEof                                        { $1 }

e :
  s                                              { Syntax.e_sexp $1 }
| LET var EQ s IN e                              { Syntax.e_let $2 $4 $6 }
| MASK LBRACKET lset RBRACKET e                  { Syntax.e_mask $3 $5 }
| MASK LBRACE lset RBRACE e                      { Syntax.e_mask $3 $5 }
;

s :
  v                                              { Syntax.s_val $1 }
| v LPARAN v RPARAN                              { Syntax.s_app $1 $3 }
| NEW                                            { Syntax.s_new ()    }
| NEWL loc                                       { Syntax.s_new_loc $2 }
| p                                              { Syntax.s_read (fst $1) (snd $1)}
| p COLONEQ v                                    { Syntax.s_write (fst $1) (snd $1) $3 }
| p LPARAN v RPARAN                              { Syntax.s_mcall (fst $1) (snd $1) $3 }
;

p :
  v DOT label                                    { ($1,$3) }
;

v :
  var                                            { Syntax.v_var $1 }
| LAMBDA LPARAN var COMMA var RPARAN DOT e       { Syntax.v_lam $3 $5 $8 }
| UNDEF                                          { Syntax.v_undef () }
| INT                                            { Syntax.v_int $1 }
;

var :
  IDENT                                          { Syntax.create_var $1 } 

label :
  IDENT                                          { Syntax.create_label $1 }

lset :
  lsetp                                          { $1 }
;

lsetp :
  loc                                            { Location.LSet.singleton $1 }
| loc lsets                                      { Location.LSet.add $1 $2 }
| loc COMMA lsets                                { Location.LSet.add $1 $3 }
;

lsets :
                                                 { Location.LSet.empty }
| loc lsets                                      { Location.LSet.add $1 $2 }
| loc COMMA lsets                                { Location.LSet.add $1 $3 }
;

loc :
  LOC                                            { $1 }
;
