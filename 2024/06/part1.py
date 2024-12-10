count = 0
seen = set()


def main():
    global grid
    with open("input.txt", "r") as file:
        grid = file.read().splitlines()

    i, j = findStart()
    move(i, j, 0, 0)
    print(len(seen))


dirs = [[-1, 0], [0, 1], [1, 0], [0, -1]]


def move(i: int, j: int, dir: int, depth: int):
    # Recursion doesn't work because python LUL
    while True:
        seen.add((i, j))
        a, b = dirs[dir]
        newi = i + a
        newj = j + b
        if newi < 0 or newi > len(grid) - 1:
            break
        if newj < 0 or newj > len(grid[i]) - 1:
            break

        nextSquare = grid[newi][newj]
        if nextSquare == "#":
            dir = (dir + 1) % len(dirs)
            depth += 1
        else:
            i = newi
            j = newj
            depth += 1


def findStart():
    i = 0
    for line in grid:
        j = line.find("^")
        if j != -1:
            return [i, j]
        i += 1

    return [-1, -1]


main()
