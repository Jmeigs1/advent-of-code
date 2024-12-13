LIBS = str,base,stdio,re

.SILENT:
run: all
	# ./part1.out
	./part2.out

all: part1 part2

part1: part1.out

part1.out: part1.ml
	ocamlfind ocamlc -linkpkg -package $(LIBS) -o $@ $^

part2: part2.out

part2.out: part2.ml
	ocamlfind ocamlc -linkpkg -package $(LIBS) -o $@ $^

clean:
	rm -f *.cmi
	rm -f *.cmo
	rm -f *.cmx
	rm -f *.out
