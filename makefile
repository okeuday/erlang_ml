#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

OCAMLC ?= ocamlc
OCAMLOPT ?= ocamlopt
OCAMLDEP ?= ocamldep
OCAMLMKLIB ?= ocamlmklib
OCAMLFLAGS = -safe-string -w @A
STDLIBDIR = $(shell $(OCAMLC) -where)
OCAMLDEPS_NUM = \
    arith_flags.cmi \
    arith_flags.cmx \
    arith_status.cmi \
    arith_status.cmx \
    big_int.cmi \
    big_int.cmx \
    int_misc.cmi \
    int_misc.cmx \
    nat.cmi \
    nat.cmx \
    num.cmi \
    num.cmx \
    ratio.cmi \
    ratio.cmx \
    libnums.a \
    nums.a \
    nums.cmxa
OCAMLDEPS=\
    nums.cmxa \
    str.cmxa

all: \
     dependency_num \
     erlang.cmi \
     erlang.cmx \
     main.cmi \
     main.cmx
	$(OCAMLOPT) -o test $(OCAMLDEPS) erlang.cmx main.cmx -ccopt -L.

clean:
	rm -f test *.cmi *.cmx *.o \
          dependency_num $(OCAMLDEPS_NUM)
	cd external/num-1.1/src && $(MAKE) clean

dependency_num:
	test -f $(STDLIBDIR)/nums.cmxa || \
    (cd external/num-1.1/src && \
     $(MAKE) OCAMLC="$(OCAMLC)" \
             OCAMLOPT="$(OCAMLOPT)" \
             OCAMLDEP="$(OCAMLDEP)" \
             OCAMLMKLIB="$(OCAMLMKLIB)" \
             nums.cmxa libnums.a && \
     cp $(OCAMLDEPS_NUM) ../../..)
	touch $@

%.cmi: %.mli
	$(OCAMLC) $(OCAMLFLAGS) -o $@ -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -o $@ -c $<

