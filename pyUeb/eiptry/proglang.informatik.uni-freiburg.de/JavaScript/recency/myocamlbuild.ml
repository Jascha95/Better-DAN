open Ocamlbuild_plugin;;
open Command;;

let packages = "unix,ocamlgraph,proglangUtils";; (* written using a comma separated list *)

let ocamlfind x = S[A"ocamlfind"; x; A"-package"; A packages];;
let ocamlfind_d x = S[A"ocamlfind"; x; A"-package"; A packages];;
(* let ocamlfind_d x = S[A"ocamlfind"; x; A"-g"; A"-package"; A packages];; *)

dispatch begin function
  | Before_options ->
      (* by using Before_options one let command line options have an higher priority *)
      (* on the contrary using After_options will guarantee to have the higher priority *)
      Options.ocamlc := ocamlfind_d& A"ocamlc";
      Options.ocamlopt := ocamlfind& A"ocamlopt";
      Options.ocamldep   := ocamlfind & A"ocamldep";
      Options.ocamldoc   := ocamlfind & A"ocamldoc";
      Options.ocamlmktop := ocamlfind & A"ocamlmktop"
  | After_rules ->
      flag ["ocaml"; "compile"] (A"-linkpkg");
      flag ["ocaml"; "link"] (A"-linkpkg");


  | _ -> ()
end;;
