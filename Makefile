OCAMLBUILD ?= ocamlbuild

default: byte

byte:
	@$(OCAMLBUILD) -use-ocamlfind test.byte
.PHONY: byte

native:
	@$(OCAMLBUILD) -use-ocamlfind test.native
.PHONY: native

indent:
	@which ocp-indent > /dev/null
	@find . \( -name \*.ml -o -name \*.mli \) -not -path ./_build/\* -exec ocp-indent -i '{}' \;
.PHONY: indent

clean:
	@$(OCAMLBUILD) -clean
.PHONY: clean
