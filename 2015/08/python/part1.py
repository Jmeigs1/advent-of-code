import ast

file = open("input.txt", "r")
count = 0

for line in file:
    line = line.strip()
    count += len(line) - (len(ast.literal_eval(line)))

print(count)
