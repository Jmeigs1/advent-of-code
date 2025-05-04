import re
from typing import List

special_char_re = re.compile(r"[^a-zA-Z0-9\.]")
is_number_re = re.compile(r"^\d+$")


def is_special_char(c: str) -> bool:
    return special_char_re.match(c) is not None


def is_number(s: str) -> bool:
    return is_number_re.match(s) is not None


def get_adjcent_tiles(i, j, data) -> List[tuple[int, int]]:
    return [
        (i + x, j + y)
        for x in range(-1, 2)
        for y in range(-1, 2)
        if not (x == 0 and y == 0)
        and (0 <= i + x < len(data))
        and (0 <= j + y < len(data[0]))
    ]


def process_tile(i, j, data, seen, results):
    adjacent = get_adjcent_tiles(i, j, data)

    for tile in adjacent:
        if tile in seen:
            continue

        ti, tj = tile
        char = data[ti][tj]

        if is_number(char):
            seen.add((ti, tj))
            before = ""
            after = ""
            for k in range(tj - 1, -1, -1):
                if not is_number(data[ti][k]):
                    break
                before = data[ti][k] + before
                seen.add((ti, k))
            for k in range(tj + 1, len(data[ti])):
                if not is_number(data[ti][k]):
                    break
                after = after + data[ti][k]
                seen.add((ti, k))
            full = before + char + after
            results.append(int(full))


def main():
    seen = set()
    results: List[int] = []
    data = open("input.txt").read().splitlines()

    for i, line in enumerate(data):
        for j, char in enumerate(line.strip()):
            if is_special_char(char):
                process_tile(i, j, data, seen, results)

    print("sum:", sum(results))


main()
