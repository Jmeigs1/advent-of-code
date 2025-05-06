from functools import reduce
import re


def get_nums(line: str):
    return re.findall(r"\d+", line)


def iter_half(num: int):
    for i in range(1, num // 2 + 1):
        yield i


def main():
    lines = open("input.txt").readlines()
    time, dist = lines

    times = get_nums(time)
    dists = get_nums(dist)

    if len(times) != len(dists):
        raise ValueError("Times and distances must be the same length")

    time = int(reduce(lambda x, y: x + y, times))
    dist = int(reduce(lambda x, y: x + y, dists))

    min_idx = next((v for v in iter_half(time) if (time - v) * v > dist), None)
    if min_idx is None:
        print(0)
        return

    count = (time - 1) - (min_idx - 1) * 2
    print(count)


main()
