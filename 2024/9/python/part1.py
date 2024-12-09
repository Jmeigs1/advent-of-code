def debug(line):
    for n in line:
        print(n, end="")
    print("")
    return


def repeat(v, times):
    return [item for item in [v] for _ in range(times)]


def doEven(line, i, depth):
    count = 0
    for j in range(depth, depth + line[i]):
        count += (i // 2) * j
    return count


def doOdd(line, i, j, depth, reg):
    count = 0
    while len(reg) < line[i] and i < j:
        reg += repeat(j // 2, line[j])
        j -= 2

    regIdx = 0
    for k in range(depth, depth + line[i]):
        count += reg[regIdx] * k
        regIdx += 1

    reg = reg[line[i] :]

    return (count, j, reg)


def doFlush(depth, reg):
    count = 0

    regIdx = 0
    for k in range(depth, depth + len(reg)):
        count += reg[regIdx] * k
        regIdx += 1

    reg = []
    return count, reg


def main():
    global count
    global grid

    count = 0
    file = open("input.txt", "r")
    line = list(map(int, list(file.read().strip())))

    i = 0
    j = len(line) - 1
    reg = []
    depth = 0

    while i <= j:
        if i % 2 == 0:
            count += doEven(line, i, depth)
        else:
            inc, j, reg = doOdd(line, i, j, depth, reg)
            count += inc

        depth += line[i]
        i += 1

    inc, reg = doFlush(depth, reg)
    count += inc

    print(count)


main()
