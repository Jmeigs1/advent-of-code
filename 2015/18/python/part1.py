def countAdj(i, j, grid):
    boundi = len(grid)
    boundj = len(grid[0])

    def isInBounds(i, j):
        return i >= 0 and j >= 0 and i < boundi and j < boundj

    dis = [-1, 0, 1]
    djs = [-1, 0, 1]

    count = 0

    for di in dis:
        for dj in djs:
            if di == 0 and dj == 0:
                continue
            if isInBounds(i + di, j + dj) and grid[i + di][j + dj] == "#":
                count += 1

    return count


def getNextState(i, j, grid):
    val = grid[i][j]
    numAdj = countAdj(i, j, grid)
    if val == "#" and numAdj not in [2, 3]:
        val = "."
    elif val == "." and numAdj == 3:
        val = "#"

    return val


def getNextGrid(grid):
    newGrid = []
    for i in range(len(grid)):
        newRow = []
        for j in range(len(grid[i])):
            newVal = getNextState(i, j, grid)
            newRow.append(newVal)
        newGrid.append(newRow)
    return newGrid


def main():
    file = open("input.txt", "r")
    grid = list(map(lambda s: list(s), file.read().strip().splitlines()))

    for _ in range(100):
        grid = getNextGrid(grid)

    count = 0
    for row in grid:
        for v in row:
            if v == "#":
                count += 1

    print(count)


main()