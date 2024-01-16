.PHONY: clean orig all

all: test_regex test_arith orig

test_regex: string_aux.ml regexp_mod.mli regexp_mod.ml test.ml
	ocamlc -o $@ $^

test_arith: arith.ml
	ocamlc -o $@ $^

%.ml: %.mll
	ocamllex $<

orig:
	ocamlc -c regexp.mli regexp.ml

clean:
	rm -f *.cmi *.cmo test_arith test_regex arith.ml
