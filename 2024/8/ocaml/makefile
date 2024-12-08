run: part1
	@./part1.out

all: part1 part2

part1: part1.out

part1.out: part1.ml
	@ocamlc $^ -o part1.out

part2: part2.out
	@./part2.out

part2.out: part2.ml
	@ocamlc $^ -o part2.out
