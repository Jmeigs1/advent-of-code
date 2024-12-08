from collections import defaultdict
from typing import Any


def findAnts(grid: list[str]):
    ants = defaultdict(list)
    for i in range(len(grid)):
        for j in range(len(grid[i])):
            val = grid[i][j]
            if val != ".":
                ants[val].append((i, j))
    return ants


def getPairs(lst: list[Any]):
    pairs = []
    for i in range(len(lst)):
        for j in range(i + 1, len(lst)):
            pairs.append((lst[i], lst[j]))
    return pairs


def getHotSpots(pair):
    spot1, spot2 = pair

    idiff = spot1[0] - spot2[0]
    jdiff = spot1[1] - spot2[1]

    hs1 = (spot1[0] + idiff, spot1[1] + jdiff)
    hs2 = (spot1[0] - 2 * idiff, spot1[1] - 2 * jdiff)

    return (hs1, hs2)


def isInBounds(spot, boundi, boundj):
    i, j = spot
    return i >= 0 and j >= 0 and i < boundi and j < boundj


def main():
    global count
    global grid

    count = 0
    file = open("input.txt", "r")
    grid = file.read().strip().split("\n")

    ants = findAnts(grid)
    keys = ants.keys()

    boundi = len(grid)
    boundj = len(grid[0])

    spots = set()

    for k in keys:
        pairs = getPairs(ants[k])
        for p in pairs:
            hs1, hs2 = getHotSpots(p)
            if isInBounds(hs1, boundi, boundj):
                spots.add(hs1)
            if isInBounds(hs2, boundi, boundj):
                spots.add(hs2)
    print(len(spots))


main()
