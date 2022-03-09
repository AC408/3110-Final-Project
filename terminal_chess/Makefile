.PHONY: test check

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/terminal_chess.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:

clean:
	dune clean

doc:
	dune build @doc
