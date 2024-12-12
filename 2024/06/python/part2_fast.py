count = 0
blocks = set()


def main():
    global grid
    with open("input.txt", "r") as file:
        grid = file.read().splitlines()

    i, j = findStart()
    move(i, j, 0, 0)

    print(len(blocks))


dirs = [[-1, 0], [0, 1], [1, 0], [0, -1]]


def getNext(i, j, dir):
    a, b = dirs[dir]
    newi = i + a
    newj = j + b
    return (newi, newj)


def move(i: int, j: int, dir: int, depth: int):
    global count
    # start = (i, j)

    seen = {}
    checked = set()
    # Recursion doesn't work because python LUL
    while True:
        seen[(i, j)] = dir
        [newi, newj] = getNext(i, j, dir)

        if newi < 0 or newi > len(grid) - 1:
            break
        if newj < 0 or newj > len(grid[i]) - 1:
            break

        nextSquare = grid[newi][newj]
        if nextSquare == "#":
            newDir = (dir + 1) % len(dirs)
            dir = newDir
        else:
            if nextSquare != "^" and (newi, newj) not in checked:
                if checkLoop(i, j, (newi, newj), dir, 0):
                    blocks.add((newi, newj))
                checked.add((newi, newj))
            i = newi
            j = newj

        depth += 1
    print(len(seen))


def checkLoop(i: int, j: int, block: tuple[int, int], dir: int, depth: int):
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
        if nextSquare == "#" or block == (newi, newj):
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
