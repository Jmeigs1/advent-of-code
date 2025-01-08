import re

replacements: list[tuple[str, str]] = []
results = set()

file = open("input.txt", "r")
p1, p2 = file.read().strip().split("\n\n")

for line in p1.splitlines():
    i, o = line.split(" => ")
    replacements.append((i, o))

for r in replacements:
    i, o = r
    locations = [m.start() for m in re.finditer(i, p2)]
    for s in locations:
        v = p2[0:s] + o + p2[s + len(i) :]
        results.add(v)

print(len(results))
