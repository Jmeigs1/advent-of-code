import re


def on(lit: set, coord):
    lit.add(coord)


def off(lit: set, coord):
    lit.discard(coord)


def toggle(lit, coord):
    if coord in lit:
        lit.discard(coord)
    else:
        lit.add(coord)


def get_op(s):
    if re.match("turn on", s) is not None:
        return on
    elif re.match("turn off", s) is not None:
        return off
    elif re.match("toggle", s) is not None:
        return toggle
    raise Exception("bad get_op")


def get_coords(s):
    def re_map(s):
        strs = s.split(",")
        return (int(strs[0]), int(strs[1]))

    matches = re.findall(r"\d+,\d+", s)
    return list(map(re_map, matches))


def do_for_coords(lit, coords, op):
    start_i = min(coords[0][0], coords[1][0])
    end_i = max(coords[0][0], coords[1][0])

    start_j = min(coords[0][1], coords[1][1])
    end_j = max(coords[0][1], coords[1][1])

    for i in range(start_i, end_i + 1):
        for j in range(start_j, end_j + 1):
            op(lit, (i, j))


count = 0
lit = set()
file = open("input.txt", "r")
for line in file:
    op = get_op(line)
    coords = get_coords(line)
    do_for_coords(lit, coords, op)

print(len(lit))