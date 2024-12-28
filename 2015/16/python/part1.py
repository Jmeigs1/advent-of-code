data = """
children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
"""

info_map = {}

for line in data.strip().splitlines():
    k, v = line.split(": ")
    info_map[k] = v

file = open("input.txt", "r")
i = 1
for line in file:
    line = line.strip()
    info = ": ".join(line.split(": ")[1:])
    data_points = info.split(", ")

    bad = False
    for d in data_points:
        k, v = d.split(": ")
        if info_map[k] != v:
            bad = True
            break

    if not bad:
        print(i)
        break
    i += 1
