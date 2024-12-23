from collections import defaultdict

count = 0

start = (0, 0)
start_robot = (0, 0)
seen = set()
seen.add(start)
dirs = {">": (0, 1), "<": (0, -1), "^": (-1, 0), "v": (1, 0)}


def add_tpl(a, b):
    return ((a[0] + b[0]), a[1] + b[1])


file = open("input.txt", "r")
line = file.read().strip()

santas_turn = True

for c in line:
    if santas_turn:
        start = add_tpl(start, dirs[c])
        seen.add(start)
    else:
        start_robot = add_tpl(start_robot, dirs[c])
        seen.add(start_robot)
    santas_turn = not santas_turn

print(len(seen))
