.PHONY: test check

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec src/main.exe

zip:

clean:
	dune clean

doc:
	dune build @doc
