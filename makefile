#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

OCAMLFLAGS=-w @A-32-27
#OCAMLFLAGS=-w @A

DEPS=\
     nums.cmxa \
     str.cmxa

all: \
     Erlang.cmi \
     Erlang.cmx \
     Main.cmx
	ocamlopt -o test $(DEPS) Erlang.cmx Main.cmx

clean:
	rm -f test *.cmi *.cmx *.o 

%.cmi: %.mli
	ocamlc -o $@ -c $<

%.cmx: %.ml
	ocamlopt $(OCAMLFLAGS) -o $@ -c $<

