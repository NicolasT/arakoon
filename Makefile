# This makefile wrapper makes DEBIAN packaging easier. 

OCAML_VERSION ?= 3.12.1
OCAML_LIBDIR ?= $(DESTDIR)/usr/lib/ocaml/
OCAML_FIND ?= ocamlfind

all: build

clean:
	ocamlbuild -clean

build: explain_macro
	ocamlbuild -use-ocamlfind arakoon.byte arakoon.native arakoon_client.cma arakoon_client.cmxa arakoon_client.a

explain_macro:
	ocamlbuild -use-ocamlfind -cflag -I -cflag +camlp4 -pp camlp4orf  explain_macro.cmo

test:
	./arakoon.native --run-all-tests

install: install_client install_server

install_server:
	mkdir -p $(DESTDIR)/usr/bin/
	cp ./arakoon.native $(DESTDIR)/usr/bin/arakoon

install_client:
	mkdir -p $(OCAML_LIBDIR)
	$(OCAML_FIND) install arakoon_client -destdir $(OCAML_LIBDIR) META \
	  _build/src/arakoon_client.cma \
	  _build/src/arakoon_client.cmxa \
	  _build/src/client/arakoon_exc.mli \
	  _build/src/client/arakoon_exc.cmi \
	  _build/src/client/arakoon_client.mli \
	  _build/src/client/arakoon_client.cmi \
	  _build/src/client/arakoon_remote_client.mli \
	  _build/src/client/arakoon_remote_client.cmi \
          _build/src/plugins/registry.mli \
          _build/src/plugins/registry.cmi \
          _build/src/tools/llio.mli \
          _build/src/tools/llio.cmi \
          _build/src/arakoon_client.a 

uninstall_client:
	$(OCAML_FIND) remove arakoon_client -destdir $(OCAML_LIBDIR)

coverage:
	ocamlbuild -use-ocamlfind \
	-tag 'package(bisect)' \
	-tag 'syntax(camlp4o)' \
	-tag 'syntax(bisect_pp)' \
	arakoon.d.byte

.PHONY: install test build install_client
         
