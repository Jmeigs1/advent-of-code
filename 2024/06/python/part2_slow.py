count = 0
blocks = set()


def main():
    global grid
    global count

    with open("input.txt", "r") as file:
        grid = file.read().splitlines()

    starti, startj = findStart()

    for i in range(len(grid)):
        for j in range(len(grid)):
            c = grid[i][j]
            if c == "#" or c == "^":
                continue
            count += 1 if checkLoop(starti, startj, (i, j), 0, 0) else 0
    print(count)


dirs = [[-1, 0], [0, 1], [1, 0], [0, -1]]


def getNext(i, j, dir):
    a, b = dirs[dir]
    newi = i + a
    newj = j + b
    return (newi, newj)


def checkLoop(i: int, j: int, extraBlock: tuple[int, int], dir: int, depth: int):
    seen: dict[tuple[int, int], int] = {}

    # Recursion doesn't work because python LUL
    while True:
        if (i, j) not in seen:
            seen[(i, j)] = dir
        else:
            if seen[(i, j)] == dir:
                return True

        [newi, newj] = getNext(i, j, dir)

        if newi < 0 or newi > len(grid) - 1:
            break
        if newj < 0 or newj > len(grid[i]) - 1:
            break

        nextSquare = grid[newi][newj]
        if nextSquare == "#" or (newi, newj) == extraBlock:
            newDir = (dir + 1) % len(dirs)
            dir = newDir
        else:
            i = newi
            j = newj

        depth += 1
    return False


def findStart():
    i = 0
    for line in grid:
        j = line.find("^")
        if j != -1:
            return [i, j]
        i += 1

    return [-1, -1]


main()