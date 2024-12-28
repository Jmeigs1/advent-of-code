import re
import itertools


def nums(s):
    return list(map(int, re.findall(r"-?\d+", s)))


def mul(xy):
    x, y = xy
    return [y * v for v in x]


def mul2(n):
    total = 1
    for i in range(0, len(n)):
        total *= max(n[i], 0)
    return total


ingredients = []

file = open("input.txt", "r")
for line in file:
    line = line.strip()
    ingredients.append(nums(line)[:-1])

combos = itertools.product(range(100), repeat=4)

best = 0

for combo in combos:
    a, b, c, d = combo
    if (a + b + c + d) == 100:
        z = zip(ingredients, combo)
        v = map(mul, z)
        v = [sum(x) for x in zip(*v)]
        if 0 in v:
            continue
        v = mul2(v)
        best = max(v, best)

print(best)
