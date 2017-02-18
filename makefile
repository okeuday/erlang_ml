#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

OCAMLFLAGS=-safe-string -w @A

DEPS=\
     nums.cmxa \
     str.cmxa

all: \
     erlang.cmi \
     erlang.cmx \
     main.cmx
	ocamlopt -o test $(DEPS) erlang.cmx main.cmx

clean:
	rm -f test *.cmi *.cmx *.o 

%.cmi: %.mli
	ocamlc $(OCAMLFLAGS) -o $@ -c $<

%.cmx: %.ml
	ocamlopt $(OCAMLFLAGS) -o $@ -c $<

