OCAMLBUILD ?= ocamlbuild

default: byte

byte:
	@$(OCAMLBUILD) -use-ocamlfind test.byte
.PHONY: byte

native:
	@$(OCAMLBUILD) -use-ocamlfind test.native
.PHONY: native

clean:
	@$(OCAMLBUILD) -clean
.PHONY: clean
