from collections import defaultdict

count = 0
file = open("input.txt", "r")
line = file.read().strip()
start = (0, 0)
seen = defaultdict(int)
seen[start] += 1
dirs = {">": (0, 1), "<": (0, -1), "^": (-1, 0), "v": (1, 0)}


def add_tpl(a, b):
    return ((a[0] + b[0]), a[1] + b[1])


for c in line:
    start = add_tpl(start, dirs[c])
    seen[start] += 1

print(len(seen))
