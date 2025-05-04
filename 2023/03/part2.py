import re
from typing import List

special_char_re = re.compile(r"[\*]")
is_number_re = re.compile(r"^\d+$")

offsets = [(i, j) for i in range(-1, 2) for j in range(-1, 2) if (i, j) != (0, 0)]


def is_in_bounds(i, j, rows, cols) -> bool:
    return 0 <= i < rows and 0 <= j < cols


def get_adjacent_tiles(i, j, rows, cols) -> List[tuple[int, int]]:
    return [
        (i + di, j + dj)
        for di, dj in offsets
        if is_in_bounds(i + di, j + dj, rows, cols)
    ]


def process_tile(i, j, data, results):
    rows, cols = len(data), len(data[0])
    adjacent = get_adjacent_tiles(i, j, rows, cols)
    seen = set()
    parts = []

    for ti, tj in adjacent:
        if (ti, tj) in seen:
            continue

        char = data[ti][tj]
        if is_number_re.match(char):
            seen.add((ti, tj))
            before, after = "", ""

            for k in range(tj - 1, -1, -1):
                if not is_number_re.match(data[ti][k]):
                    break
                before = data[ti][k] + before
                seen.add((ti, k))

            for k in range(tj + 1, cols):
                if not is_number_re.match(data[ti][k]):
                    break
                after += data[ti][k]
                seen.add((ti, k))

            full = before + char + after
            parts.append(int(full))

    if len(parts) == 2:
        results.append(parts[0] * parts[1])


def main():
    results: List[int] = []
    with open("input.txt") as file:
        data = [line.strip() for line in file]

    rows, cols = len(data), len(data[0]) if data else 0
    for i in range(rows):
        for j in range(cols):
            if special_char_re.match(data[i][j]):
                process_tile(i, j, data, results)

    print("sum:", sum(results))


main()
