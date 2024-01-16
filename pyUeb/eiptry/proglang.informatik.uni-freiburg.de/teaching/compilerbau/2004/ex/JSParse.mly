%{
  open LexemeDefinition;;
  open AST;;

let fst_of_triplet (a, _, _) = a;;
let snd_of_triplet (_, a, _) = a;;
let trd_of_triplet (_, _, a) = a;;

let string_of_constant c = match c with
  Number f  -> string_of_float f
| String s  -> s
| True      -> "true"
| False     -> "false"
| RegExp s  -> s
| This      -> "this"
| Undefined -> ""
| Null      -> "null";;

%}

/* Tokens revisited */
/* Values */
%token Ltrue
%token Lfalse
%token <int> Lint
%token <float> Lfloat
%token <char> Lchar
%token <string> Lstring
%token <string> Lident
%token Lnull
%token Leof

/* to be ignored */
%token Lwhitespace 
%token <string> Lcomment
  
/* Keywords */
%token KWbreak KWcase KWcatch 
%token KWcontinue KWdefault KWdelete
%token KWdo KWelse KWfinally
%token KWfor KWfunction KWif 
%token KWin KWinstanceof KWnew 
%token KWreturn KWswitch KWthis
%token KWthrow KWtry KWtypeof 
%token KWvar KWvoid KWwhile 
%token KWwith

/* Future reserved words */
%token FRWabstract FRWboolean FRWbyte 
%token FRWchar FRWclass FRWconst 
%token FRWdebugger FRWenum FRWexport 
%token FRWextends FRWfinal FRWfloat 
%token FRWgoto FRWimplements FRWint 
%token FRWinterface FRWlong FRWnative
%token FRWpackage FRWprivate FRWprotected
%token FRWshort FRWstatic FRWsuper
%token FRWsynchronized FRWthrows FRWtransient
%token FRWvolatile

/* strucutral information (parenthesises, comma, dot, semicolon) */
%token Llparen Lrparen Llbrace 
%token Lrbrace Llbracket Lrbracket 
%token Lsemicolon Lcomma Ldot Lhook Lcolon

/* Assignments and operators */
%token Lassign Lgreater Lless Lbang
%token Ltilde
%token Leq Lle Lge Lne
%token Leqq Lneq Lsc_or
%token Lsc_and Lincr Ldecr 
%token Lplus Lminus Lstar 
%token Lslash Lbit_and Lbit_or 
%token Lxor Lrem Llshift 
%token Lrsignedshift Lrunsignedshift Lplusassign
%token Lminusassign Lstarassign Lslashassign
%token Landassign Lorassign Lxorassign
%token Lremassign Llshiftassign 
%token Lrsignedshiftassign Lrunsignedshiftassign


/* Startsymbol */
%start program
%type <AST.program> program


/* ******************** */
/*    Toplevel-Rules    */
/* ******************** */


/* Productions */

%%
program :
   source_elements Leof {Program $1} 
;


/* Source Elements */

source_elements :
  Leof                           {[]}
| source_element                 {[$1]}
| source_elements source_element {$1 @ [$2]}
;

source_element :
  statement            {Statement $1} 
| function_declaration {$1}
;


/* Function declarations */

function_declaration :
  KWfunction Lident Llparen formal_parameter_list Lrparen Llbrace function_body Lrbrace 
    {Function_declaration ((Some (Identifier $2)), $4, $7)}
;

function_expression :
  KWfunction Llparen formal_parameter_list Lrparen Llbrace function_body Lrbrace        
    {Function_expression (None, $3, $6)}
| KWfunction Lident Llparen formal_parameter_list Lrparen Llbrace function_body Lrbrace 
    {Function_expression ((Some (Identifier $2)), $4, $7)}
;

formal_parameter_list :
                                      {[]}
| Lident                              {[Identifier $1]}
| formal_parameter_list Lcomma Lident {$1 @ [Identifier $3]}
;

function_body :
  source_elements {$1}
;



/* ************** */
/*   Statements   */
/* ************** */


/* Top-Level for statements */

statement :
  block                {$1}
| variable_statement   {$1}
| empty_statement      {$1}
| expression_statement {$1}
| if_statement         {$1}
| iteration_statement  {$1}
| continue_statement   {$1}
| break_statement      {$1}
| return_statement     {$1}
| with_statement       {$1}
| labelled_statement   {$1}
| switch_statement     {$1}
| throw_statement      {$1}
| try_statement        {$1}
;


/* Block */

block :
  Llbrace optional_statement_list Lrbrace     {Block $2}
;


optional_statement_list :
                                     {[]}
| non_empty_statement_list           {$1}
;

non_empty_statement_list :
  statement                          {[$1]}
| non_empty_statement_list statement {$1 @ [$2]}
;


/* Variable Statement */

variable_statement :
  KWvar variable_declaration_list Lsemicolon {Variable_declaration $2}
;

variable_declaration_list :
  variable_declaration                                  {[$1]}
| variable_declaration_list Lcomma variable_declaration {$1 @ [$3]}
;

variable_declaration_list_no_in :
  variable_declaration_no_in                                        {[$1]}
| variable_declaration_list_no_in Lcomma variable_declaration_no_in {$1 @ [$3]}
;
		
variable_declaration :
  Lident             {((Identifier $1), None)}
| Lident initialiser {((Identifier $1), Some $2)} 
;

variable_declaration_no_in :
  Lident                   {((Identifier $1), None)}
| Lident initialiser_no_in {((Identifier $1), Some $2)} 
;

initialiser :
  Lassign assignment_expression {$2}
;

initialiser_no_in :
  Lassign assignment_expression_no_in {$2}
;


/* Skip */

empty_statement :
  Lsemicolon {Skip}
;


/* Expression */

expression_statement :
  expression Lsemicolon {Expression $1}
;
	

/* Conditionals  */

if_statement :
  KWif Llparen expression Lrparen statement KWelse statement {If ($3, $5, Some $7)}
| KWif Llparen expression Lrparen statement                  {If ($3, $5, None)}
;


switch_statement :
  KWswitch Llparen expression Lrparen case_block {Switch ($3, (fst_of_triplet $5), (snd_of_triplet $5),
                                                          (trd_of_triplet $5))}
;

case_block :
  Llbrace case_clauses Lrbrace                             {($2, None, [])}
| Llbrace Lrbrace                                          {([], None, [])}
| Llbrace default_clause Lrbrace                           {([], Some $2, [])}
| Llbrace case_clauses default_clause Lrbrace              {($2, Some $3, [])}
| Llbrace default_clause case_clauses Lrbrace              {([], Some $2, $3)}
| Llbrace case_clauses default_clause case_clauses Lrbrace {($2, Some $3, $4)}
/* The case_clauses after the default_clause are treated similar 
   to the ones before. But if there's a satisfied expression in 
   the first list and no break in the first list but one in the
   default case, than the second list is not evaluated. One small
   difference, but we have to keep them in separate lists. */
;

case_clauses :
  case_clause              {[$1]}   
| case_clauses case_clause {$1 @ [$2]}
;

case_clause :
  KWcase expression Lcolon                          {($2, None)}
| KWcase expression Lcolon non_empty_statement_list {($2, Some $4)}
;

default_clause :
  KWdefault Lcolon                          {None}
| KWdefault Lcolon non_empty_statement_list {Some $3}
;


/* Iterations */

iteration_statement :
  KWdo statement KWwhile Llparen expression Lrparen Lsemicolon {Do ($2, $5)}
| KWwhile Llparen expression Lrparen statement                 {While ($3, $5)}
| KWfor Llparen for_bracket Lrparen statement                  {For ($3, $5)}
;

for_bracket :
  optional_expression_no_in Lsemicolon optional_expression Lsemicolon optional_expression             
    {Regular ($1, $3, $5)}
| KWvar variable_declaration_list_no_in Lsemicolon optional_expression Lsemicolon optional_expression 
    {Regular_var ($2, $4, $6)}
| left_hand_side_expression KWin expression                                                           
    {With_in ($1, $3)}
| KWvar variable_declaration_no_in KWin expression                                                    
    {With_in_and_var ($2, $4)}
;

optional_expression : 
             {None} 
| expression {Some $1}
;

optional_expression_no_in :
                   {None} 
| expression_no_in {Some $1}
;


/* Keyword-Statements */

continue_statement :
  KWcontinue /* no line terminator here */ Lident Lsemicolon {Continue (Some (Identifier $2))}
| KWcontinue /* no line terminator here */ Lsemicolon        {Continue None}
;

break_statement :
  KWbreak /* no line terminator here */ Lident Lsemicolon {Break (Some (Identifier $2))}
| KWbreak /* no line terminator here */ Lsemicolon        {Break None}
;
		
return_statement :
  KWreturn /* no line terminator here */ expression Lsemicolon {Return (Some $2)}
| KWreturn /* no line terminator here */ Lsemicolon            {Return None}
;

with_statement :
  KWwith Llparen expression Lrparen statement {With ($3, $5)}
;


/* Other Control Structures */

labelled_statement :
  Lident Lcolon statement {Labelled_statement ((Identifier $1), $3)}
;

throw_statement :
  KWthrow /* no line terminator here */ expression Lsemicolon {Throw $2}
;

try_statement :
  KWtry block catch_prod              {Try_catch_finally ($2, (Some $3), None)}
| KWtry block finally_prod            {Try_catch_finally ($2, None, (Some $3))}
| KWtry block catch_prod finally_prod {Try_catch_finally ($2, (Some $3), (Some $4))} 
;

catch_prod :
  KWcatch Llparen Lident Lrparen block {((Identifier $3), $5)}
;

finally_prod :
  KWfinally block {$2}
;


/* **************** */
/*   Expressions    */
/* **************** */


/* Top-Level Expression structure */
         
expression :
  sequence_of_expression       {Sequence $1};
expression_no_in :
  sequence_of_expression_no_in {Sequence $1};

sequence_of_expression :
  assignment_expression                                           {[$1]}
| sequence_of_expression Lcomma assignment_expression             {$1 @ [$3]};
sequence_of_expression_no_in :
  assignment_expression_no_in                                     {[$1]}
| sequence_of_expression_no_in Lcomma assignment_expression_no_in {$1 @ [$3]};

member_expression :
  primary_expression                               {$1}
| function_expression                              {$1}
| member_expression Llbracket expression Lrbracket {Array_access ($1, $3)}
| member_expression Ldot Lident                    {Object_access ($1, (Identifier $3))}
| KWnew member_expression arguments                {New_construct ($2, $3)};

/* Basic Expressions */

primary_expression : 
  KWthis                     {Constant This}
| Lident                     {Variable (Identifier $1)}
| literal                    {Constant $1}
| array_literal              {Array_construction $1}
| object_literal             {Object_construction $1}
| Llparen expression Lrparen {$2};

literal :
  numeric_literal {$1}
| Lnull           {Null}
| Ltrue           {True}
| Lfalse          {False}
| Lstring         {String $1} 
;

numeric_literal : 
  Lfloat   {Number $1}
| Lint     {Number (float_of_int $1)}
;

array_literal :
  Llbracket elision_option Lrbracket                     {$2}
| Llbracket element_list Lrbracket                       {$2}
| Llbracket element_list Lcomma elision_option Lrbracket {$2}
;
    
elision_option : 
                        {[]}
| Lcomma                {[None]}
| elision_option Lcomma {$1 @ [None]}

element_list : 
  elision_option assignment_expression                     {$1 @ [Some $2]}
| element_list Lcomma elision_option assignment_expression {$1 @ $3 @ [Some $4]}
;

object_literal :
  Llbrace Lrbrace                              {[]} 
| Llbrace property_name_and_value_list Lrbrace {$2}
;

property_name_and_value_list :
  property_name Lcolon assignment_expression                                     {[($1, $3)]}
| property_name_and_value_list Lcomma property_name Lcolon assignment_expression {$1 @ [($3, $5)]}
;

property_name :
  Lident             {(DynamicName (Identifier $1))}
| Lstring            {(StaticName (String $1))}
| numeric_literal    {(StaticName $1)}
;

new_expression :
  member_expression         {$1}
| KWnew new_expression      {New_expression $2};

call_expression :
  member_expression arguments                           {Function_call ($1, $2)}
| call_expression arguments                             {Function_call ($1, $2)}
| call_expression Llbracket expression Lrbracket        {Array_access ($1, $3)}
| call_expression Ldot Lident                           {Object_access ($1, (Identifier $3))};

arguments :
  Llparen Lrparen               {[]}
| Llparen argument_list Lrparen {$2}
;

argument_list :
  assignment_expression                      {[$1]}
| argument_list Lcomma assignment_expression {$1 @ [$3]}
;

left_hand_side_expression :
  new_expression       {$1}
| call_expression      {$1};

/* Operations */

postfix_expression :
  left_hand_side_expression                                          {$1}
| left_hand_side_expression /* no line terminator here */ Lincr      {Unop ($1, Incr_postfix)}
| left_hand_side_expression /* no line terminator here */ Ldecr      {Unop ($1, Decr_postfix)};

unary_expression :
  postfix_expression             {$1}
| KWdelete unary_expression      {Unop ($2, Delete)}
| KWvoid unary_expression        {Unop_without_sideeffect ($2, Void)}
| KWtypeof unary_expression      {Unop_without_sideeffect ($2, Typeof)}
| Lincr unary_expression         {Unop ($2, Incr_prefix)}
| Ldecr unary_expression         {Unop ($2, Decr_prefix)}
| Lplus unary_expression         {Unop_without_sideeffect ($2, Positive)}
| Lminus unary_expression        {Unop_without_sideeffect ($2, Negative)}
| Ltilde unary_expression        {Unop_without_sideeffect ($2, Tilde)}
| Lbang unary_expression         {Unop_without_sideeffect ($2, Bang)};

multiplicative_expression :
  unary_expression                                            {$1}
| multiplicative_expression Lstar unary_expression            {Binop ($1, Star, $3)}
| multiplicative_expression Lslash unary_expression           {Binop ($1, Slash, $3)}
| multiplicative_expression Lrem unary_expression             {Binop ($1, Rem, $3)};

additive_expression :
  multiplicative_expression                                      {$1}
| additive_expression Lplus multiplicative_expression            {Binop ($1, Plus, $3)}
| additive_expression Lminus multiplicative_expression           {Binop ($1, Minus, $3)};

shift_expression :
  additive_expression                                            {$1}
| shift_expression Llshift additive_expression                   {Binop ($1, Lshift, $3)}
| shift_expression Lrsignedshift additive_expression             {Binop ($1, Rsignedshift, $3)}
| shift_expression Lrunsignedshift additive_expression           {Binop ($1, Runsignedshift, $3)};

relational_expression :
  shift_expression                                              {$1}
| relational_expression Lless shift_expression                  {Binop ($1, Less, $3)}
| relational_expression Lgreater shift_expression               {Binop ($1, Greater, $3)}
| relational_expression Lle shift_expression                    {Binop ($1, Le, $3)}
| relational_expression Lge shift_expression                    {Binop ($1, Ge, $3)}
| relational_expression KWinstanceof shift_expression           {Binop ($1, Instanceof, $3)}
| relational_expression KWin shift_expression                   {Binop ($1, In, $3)};
relational_expression_no_in :
  shift_expression                                              {$1}
| relational_expression_no_in Lless shift_expression            {Binop ($1, Less, $3)}
| relational_expression_no_in Lgreater shift_expression         {Binop ($1, Greater, $3)}
| relational_expression_no_in Lle shift_expression              {Binop ($1, Le, $3)}
| relational_expression_no_in Lge shift_expression              {Binop ($1, Ge, $3)}
| relational_expression_no_in KWinstanceof shift_expression     {Binop ($1, Instanceof, $3)};
/* No KWin - rule! */

equality_expression :
  relational_expression                                      {$1}
| equality_expression Leq relational_expression              {Binop ($1, Eq, $3)}
| equality_expression Lne relational_expression              {Binop ($1, Ne, $3)}
| equality_expression Leqq relational_expression             {Binop ($1, Eqq, $3)}
| equality_expression Lneq relational_expression             {Binop ($1, Neq, $3)};
equality_expression_no_in :
  relational_expression_no_in                                {$1}
| equality_expression_no_in Leq relational_expression_no_in  {Binop ($1, Eq, $3)}
| equality_expression_no_in Lne relational_expression_no_in  {Binop ($1, Ne, $3)}
| equality_expression_no_in Leqq relational_expression_no_in {Binop ($1, Eqq, $3)}
| equality_expression_no_in Lneq relational_expression_no_in {Binop ($1, Neq, $3)};

bitwise_and_expression :
  equality_expression                                             {$1}
| bitwise_and_expression Lbit_and equality_expression             {Binop ($1, Bit_and, $3)};
bitwise_and_expression_no_in :
  equality_expression_no_in                                       {$1}
| bitwise_and_expression_no_in Lbit_and equality_expression_no_in {Binop ($1, Bit_and, $3)};

bitwise_xor_expression :
  bitwise_and_expression                                         {$1}
| bitwise_xor_expression Lxor bitwise_and_expression             {Binop ($1, Xor, $3)};
bitwise_xor_expression_no_in :
  bitwise_and_expression_no_in                                   {$1}
| bitwise_xor_expression_no_in Lxor bitwise_and_expression_no_in {Binop ($1, Xor, $3)};

bitwise_or_expression :
  bitwise_xor_expression                                           {$1}
| bitwise_or_expression Lbit_or bitwise_xor_expression             {Binop ($1, Bit_or, $3)};
bitwise_or_expression_no_in :
  bitwise_xor_expression_no_in                                     {$1}
| bitwise_or_expression_no_in Lbit_or bitwise_xor_expression_no_in {Binop ($1, Bit_or, $3)};

logical_and_expression :
  bitwise_or_expression                                            {$1}
| logical_and_expression Lsc_and bitwise_or_expression             {Binop ($1, Sc_and, $3)};
logical_and_expression_no_in :
  bitwise_or_expression_no_in                                      {$1}
| logical_and_expression_no_in Lsc_and bitwise_or_expression_no_in {Binop ($1, Sc_and, $3)};
	
logical_or_expression :
  logical_and_expression                                          {$1}
| logical_or_expression Lsc_or logical_and_expression             {Binop ($1, Sc_or, $3)};
logical_or_expression_no_in :
  logical_and_expression_no_in                                    {$1}
| logical_or_expression_no_in Lsc_or logical_and_expression_no_in {Binop ($1, Sc_or, $3)};

conditional_expression :
  logical_or_expression                                                                            
    {$1}
| logical_or_expression Lhook assignment_expression Lcolon assignment_expression                   
    {Conditional ($1, $3, $5)};
conditional_expression_no_in :
  logical_or_expression_no_in                                                                      
    {$1}
| logical_or_expression_no_in Lhook assignment_expression_no_in Lcolon assignment_expression_no_in 
    {Conditional ($1, $3, $5)};


/* Assignment Expressions */

assignment_expression :
  conditional_expression                                                        {$1}
| left_hand_side_expression assignment_operator assignment_expression           {Assign ($1, $2, $3)};
assignment_expression_no_in :
  conditional_expression_no_in                                                  {$1}
| left_hand_side_expression assignment_operator assignment_expression_no_in     {Assign ($1, $2, $3)};

assignment_operator :
  Lassign               {Regular_assign}
| Lstarassign           {Star_assign}
| Lslashassign          {Slash_assign}
| Lremassign            {Rem_assign}
| Lplusassign           {Plus_assign}
| Lminusassign          {Minus_assign}
| Llshiftassign         {Lshift_assign}
| Lrsignedshiftassign   {Rsignedshift_assign}
| Lrunsignedshiftassign {Runsignedshift_assign}
| Landassign            {And_assign}
| Lxorassign            {Xor_assign}
| Lorassign             {Or_assign}
;
%%
