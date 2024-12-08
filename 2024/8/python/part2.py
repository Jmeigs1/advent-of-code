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


def getHotSpots(pair, boundi, boundj):
    spot1, spot2 = pair

    idiff = spot1[0] - spot2[0]
    jdiff = spot1[1] - spot2[1]

    spots = [spot1]

    i = 1
    while True:
        newSpot = (spot1[0] + idiff * i, spot1[1] + jdiff * i)
        if isInBounds(newSpot, boundi, boundj):
            spots.append(newSpot)
        else:
            break
        i += 1

    i = 1
    while True:
        newSpot = (spot1[0] - idiff * i, spot1[1] - jdiff * i)
        if isInBounds(newSpot, boundi, boundj):
            spots.append(newSpot)
        else:
            break
        i += 1

    return spots


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
            hotspots = getHotSpots(p, boundi, boundj)
            for s in hotspots:
                spots.add(s)
    print(len(spots))


main()
