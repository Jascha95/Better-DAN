open Inf
open Gen
open Parser
open ProglangUtils

type p = {
  e: Syntax.e;
  se: string;
  sc: string;
  r: Gen.result;
  c: Gen.changed;
}

let token_to_string = function
  | LET -> "let"
  | IN -> "in"
  | NEW -> "new"
  | UNDEF -> "undef"
  | MASK -> "M_"
  | DOT -> "."
  | EQ -> "="
  | COLONEQ -> ":="
  | LPARAN -> "("
  | RPARAN -> ")"
  | COMMA -> ","
  | LAMBDA -> "\\"
  | IDENT s -> s
  | LEof -> ""
  | INT i -> string_of_int i
  | LOC l -> Location.Loc.string_of l
  | LBRACE -> "{"
  | RBRACE -> "}"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | NEWL -> "new_"

let parse k l = 
  let _ = Log.full_debug (lazy "Parse Program") in
  let e = Parser.prog Lexer.token l in
  let _ = Log.full_debug (lazy "Program succesfully parsed") in
  let s = Syntax.string_of_e e in
  let _ = Log.debug (lazy s) in
    k e s


let gen k =
  parse 
      (fun e se ->
         let _ = Log.full_debug (lazy "Creating Constraints") in
         let ls = Syntax.collect_locs e in
           Vars.LSVar.set_upper_for_all ls;
           let tenv = TEnv.empty in
           let lev = Vars.LEVar.create () in
           let r,c = Gen.create_e tenv lev e in
           let sc = Const.string_of () in
           let _ = Log.full_debug (lazy "Constraints created") in
           let _ = Log.debug (lazy sc) in
             k {e = e; se = se; sc = sc; r = r; c = c})

let solve k =
  gen 
    (fun p -> 
       let _ = Log.full_debug (lazy "Start solving constraints") in
         Const.solve ();
         let _ = Log.full_debug (lazy "Solving constraints finised") in
           (* Check if solution is correct *)
           k p)
