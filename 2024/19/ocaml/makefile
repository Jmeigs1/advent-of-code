LIBS = str,stdio,re,core

SRCS=$(wildcard *.ml)
OBJS=$(SRCS:.c=.out)

.SILENT:
run: all
	./part1.out
	# ./part2.out

all: part1.out part2.out

part1.out: part1.ml
	ocamlfind ocamlopt -linkpkg -package $(LIBS) -o $@ $^

part2.out: part2.ml
	ocamlfind ocamlopt -linkpkg -package $(LIBS) -o $@ $^

clean:
	rm -f *.cmi
	rm -f *.cmo
	rm -f *.cmx
	rm -f *.out
	rm -f *.o
