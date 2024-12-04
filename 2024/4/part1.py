def testDirection(
    i: int, j: int, iDir: int, jDir: int, word: str, data: list[list[str]]
):
    if iDir == 0 and jDir == 0:
        return False

    size = len(word)
    iBound = i + (size - 1) * iDir
    jBound = j + (size - 1) * jDir
    if iBound >= len(data) or iBound < 0:
        return False

    if jBound >= len(data[i]) or jBound < 0:
        return False

    for h in range(len(word)):
        comp = data[i + iDir * h][j + jDir * h]
        if comp != word[h]:
            return False

    return True


def testIndex(i: int, j: int, data: list[list[str]]):
    xDir = [-1, 0, 1]
    yDir = [-1, 0, 1]
    word = "XMAS"
    count = 0

    for x in xDir:
        for y in yDir:
            count += 1 if testDirection(i, j, x, y, word, data) else 0

    return count


def main():
    data: list[list[str]] = []
    count = 0
    for line in open("input.txt").readlines():
        data.append(list(line))

    for i in range(len(data)):
        for j in range(len(data[i])):
            v = data[i][j]
            if v == "X":
                count += testIndex(i, j, data)

    print(count)


main()
