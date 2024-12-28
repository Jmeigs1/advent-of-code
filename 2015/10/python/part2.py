import itertools

file = open("input.txt", "r")
minD = None

graph = {}
nodes = set()

for line in file:
    line = line.strip()
    p1, dist = line.split(" = ")
    a, b = p1.split(" to ")
    graph[(a, b)] = int(dist)
    graph[(b, a)] = int(dist)
    nodes.add(a)
    nodes.add(b)

for p in itertools.permutations(nodes):
    pairs = zip(p[:-1], p[1:])
    distances = map(lambda v: graph[tuple(v)], pairs)
    if minD is None:
        minD = sum(distances)
    else:
        minD = max(minD, sum(distances))

print(minD)
