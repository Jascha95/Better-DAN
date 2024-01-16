open Regexp_mod

(* Identifiers *)

let digit_syms = symclass_range '0' '9'
let digit = symclass_pos digit_syms

let letter_syms = symclass_union (symclass_range 'a' 'z') 
                                 (symclass_range 'A' 'Z')
let letter = symclass_pos letter_syms

let ident_start = alternate_list [letter; symbol '$'; symbol '_']
let ident_part = alternate ident_start digit
let ident = concat ident_start (repeat ident_part)

(* String literals
   regular expression: "([^"\\]|\\.)*  (add missing double quote!)
*)
let any = symclass_range (Char.chr 0) (Char.chr 255)

let string_lit = concat_list [
  symbol '"';
  repeat (
    alternate (symclass_neg (symclass_symbols ['"'; '\\']))
              (concat_list [symbol '\\'; symclass_pos any]);
  );
  symbol '"']
  

let test re s expected = 
   if matches re (String_aux.list_of_string s) = expected 
     then print_endline (s ^ ": OK")
     else print_endline (s ^ ": FAIL")
    
let _ =
  test digit "0" true;
  test digit "a" false;
  test letter "0" false;
  test letter "a" true;
  test ident "a1$_" true;
  test ident "a1$_" true;
  test ident "$1a_" true;
  test ident "_1$a" true;
  test ident "1_$a" false;
  test ident "_ a" false;
  test string_lit "\"\"" true;
  test string_lit "\"a1 $ \"" true;
  test string_lit "\"a1 \\\"$\\\" \"" true;
  test string_lit "a\"b\"" false;
  test string_lit "\"a\"b\"" false;
  test string_lit "\"a" false;
