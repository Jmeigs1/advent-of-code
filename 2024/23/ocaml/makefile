LIBS = str,stdio,re,core

SRCS=$(wildcard libs/*.ml)

.SILENT:
run: all
	./part1.out
	# ./part2.out

all: part1.out part2.out

part1.out: $(SRCS) part1.ml
	ocamlfind ocamlopt -linkpkg -package $(LIBS) -o $@ -I libs $^

part2.out: $(SRCS) part2.ml
	ocamlfind ocamlopt -linkpkg -package $(LIBS) -o $@ -I libs $^

clean:
	rm -f **/*.cmi *.cmi
	rm -f **/*.cmo *.cmo
	rm -f **/*.cmx *.cmx
	rm -f **/*.out *.out
	rm -f **/*.o *.o
