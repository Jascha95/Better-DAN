open Ocamlbuild_plugin;;
open Command;;

let packages = "unix";; (* written using a comma separated list *)

let ocamlfind x = S[A"ocamlfind"; x];;

dispatch begin function
  | Before_options ->
      (* by using Before_options one let command line options have an higher priority *)
      (* on the contrary using After_options will guarantee to have the higher priority *)
      Options.ocamlc := ocamlfind& A"ocamlc";
      Options.ocamlopt := ocamlfind& A"ocamlopt";
  | After_rules ->
      flag ["ocaml"; "link"] (A"-linkpkg");

      flag ["use_unix"] (S[A"-package"; A packages]);

  | _ -> ()
end;;
