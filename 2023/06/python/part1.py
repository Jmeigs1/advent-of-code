from functools import reduce
import re


def get_nums(line: str):
    return list(map(int, re.findall(r"\d+", line)))


def iter_half(num: int):
    for i in range(num // 2, 0, -1):
        mirrored = not (i * 2 == num)

        yield (
            i,
            2 if mirrored else 1,
        )


def main():
    lines = open("input.txt").readlines()
    time, dist = lines

    times = get_nums(time)
    dists = get_nums(dist)

    if len(times) != len(dists):
        raise ValueError("Times and distances must be the same length")

    totals = []
    for i in range(len(times)):
        count = 0
        dist = dists[i]
        time = times[i]

        for v, c in iter_half(time):
            d = (time - v) * v
            # print(f"v: {v}, c: {c}, d: {d}, dist: {dist}")
            if d > dist:
                count += c

        totals.append(count)

    # print(totals)
    product = reduce(lambda x, y: x * y, totals)
    print(product)


main()
