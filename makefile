#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

OCAMLC ?= ocamlc
OCAMLOPT ?= ocamlopt
OCAMLDEP ?= ocamldep
OCAMLMKLIB ?= ocamlmklib
OCAMLFLAGS = -safe-string -w @A
STDLIBDIR = $(shell $(OCAMLC) -where)
OCAMLDEPS_ZARITH = \
    big_int_Z.cmi \
    big_int_Z.cmx \
    libzarith.a \
    q.cmi \
    q.cmx \
    zarith.a \
    zarith.cma \
    zarith.cmxa \
    zarith_top.cma \
    zarith_top.cmi \
    zarith_version.cmi \
    zarith_version.cmx \
    z.cmi \
    z.cmx
OCAMLDEPS=\
    zarith.cmxa \
    str.cmxa

all: \
     dependency_zarith \
     erlang.cmi \
     erlang.cmx \
     main.cmi \
     main.cmx
	$(OCAMLOPT) -o tests $(OCAMLDEPS) erlang.cmx main.cmx -ccopt -L.

clean:
	rm -f tests *.cmi *.cmx *.o \
          dependency_zarith $(OCAMLDEPS_ZARITH)
	cd external/zarith-1.12 && $(MAKE) clean || exit 0

dependency_zarith:
	(cd external/zarith-1.12 && \
     ./configure && \
     $(MAKE) && \
     cp $(OCAMLDEPS_ZARITH) ../..)
	touch $@

erlang.cmi: lib/erlang.mli
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -c $<

erlang.cmx: lib/erlang.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c $<

main.cmi: test/main.mli
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -c $<

main.cmx: test/main.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c $<

