SOURCE := extList.mli extSSet.mli extUtils.mli log.mli \
	oUnit.mli test.mli \
	extSMap.mli extString.mli hiddenInts.mli \
	path.mli string_of.mli option.mli \
	ownMap.mli historyMap.mli prioQueues.mli

SOURCE_ML = $(SOURCE:%.mli=%.ml) 
SOURCE_CMI = $(SOURCE:%.mli=%.cmi) 
SOURCE_CMX = $(SOURCE:%.mli=%.cmx) 

MLI = $(SOURCE:%=_build/%)
CMI = $(SOURCE_CMI:%=_build/%)
CMX = $(SOURCE_CMX:%=_build/%)

.PHONY: both
both: byte native

byte:
	ocamlbuild -j 2 proglangUtils.cma

native:
	ocamlbuild -j 2 proglangUtils.cmxa

d:
	rm -rf docdir/
	ocamlbuild -j 2 \
	     proglangUtils.docdir/index.html
	 cp -R proglangUtils.docdir/ docdir

s:
	firefox docdir/index.html

ds: d
	firefox docdir/index.html

.PHONY: clean
clean:
	rm -rf _build/

install: both
	ocamlfind install proglangUtils META \
	_build/proglangUtils.cma \
	_build/proglangUtils.cmxa \
	_build/proglangUtils.a \
	_build/proglangUtils.cmi \
	$(CMI) \
	$(MLI) \
	$(CMX)

bytei: byte
	ocamlfind install proglangUtils META \
	_build/proglangUtils.cma \
	_build/proglangUtils.cmi \
	$(CMI) \
	$(MLI)

uninstall:
	ocamlfind remove proglangUtils 

test:
	ocamlbuild -j 2 main_test.byte
	./main_test.byte