OCAMLBUILD ?= ocamlbuild
OCP_INDENT ?= ocp-indent

default: byte

byte:
	@$(OCAMLBUILD) -use-ocamlfind test.byte
.PHONY: byte

native:
	@$(OCAMLBUILD) -use-ocamlfind test.native
.PHONY: native

indent:
	@which $(OCP_INDENT) > /dev/null
	@find . \( -name \*.ml -o -name \*.mli \) -not -path ./_build/\* -exec $(OCP_INDENT) -i '{}' \;
.PHONY: indent

clean:
	@$(OCAMLBUILD) -clean
.PHONY: clean
