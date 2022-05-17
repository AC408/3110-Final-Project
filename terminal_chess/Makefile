.PHONY: test check

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	zip -r ms1.zip . -x@exclude.lst

clean:
	dune clean

doc:
	dune build @doc

docs:
	dune build @doc
