from collections import defaultdict


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


def doOdd(line, i, shifts, depth):
    count = 0
    lst = shifts[i]
    for j in lst:
        reps = line[j]
        for k in range(depth, depth + reps):
            count += (j // 2) * k
        depth = depth + reps

    return count


def getShift(line, i, shifts):
    j = 1
    while j < i:
        if j in shifts:
            lst = shifts[j]
            size = sum(map(lambda x: line[x], lst))
            if line[j] >= size + line[i]:
                return i, j
            j += 2
            continue
        elif line[j] >= line[i]:
            return i, j
        j += 2
    return None


def buildShifts(line):
    shifts = defaultdict(list)
    ignore = set()

    i = len(line) - 1
    while not i < 0:
        shift = getShift(line, i, shifts)
        if shift is not None:
            si, sj = shift
            shifts[sj].append(si)
            ignore.add(si)
        i -= 2
    return shifts, ignore


def main():
    global count
    global grid

    count = 0
    file = open("input.txt", "r")
    line = list(map(int, list(file.read().strip())))

    i = 0
    depth = 0

    shifts, ignore = buildShifts(line)

    while i < len(line) - 1:
        if i % 2 == 0:
            if i not in ignore:
                count += doEven(line, i, depth)
        else:
            if i in shifts:
                count += doOdd(line, i, shifts, depth)

        depth += line[i]
        i += 1

    print(count)


main()
