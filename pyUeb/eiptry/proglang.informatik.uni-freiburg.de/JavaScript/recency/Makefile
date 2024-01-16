byte:
#	ocamlbuild  -classic-display -j 2 main.byte
	ocamlbuild -j 2 main.byte
	ln -s _build/main.byte run

#        -Is languages,languages/deutsch 
native:
	ocamlbuild -j 2 main.native
	ln -s _build/main.native run

d:
	rm -rf docdir/
	ocamlbuild -j 2 \
	     main.docdir/index.html
	 cp -R main.docdir/ docdir

ds: d
	firefox docdir/index.html

clean:
	rm -rf _build/

test: byte
	./run -t
#	OCAMLRUNPARAM=v=63 ./run -t

l:
	make | grep line