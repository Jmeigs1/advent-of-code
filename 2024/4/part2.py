def testIndex(i: int, j: int, data: list[list[str]]):
    count = 0

    word1 = ""
    for h in [-1, 0, 1]:
        word1 += data[i + h][j + h]
    word2 = ""
    for h in [-1, 0, 1]:
        word2 += data[i + h][j - h]
    word3 = word1[::-1]
    word4 = word2[::-1]

    for w in [word1, word2, word3, word4]:
        if w == "MAS":
            count += 1

    return count > 1


def main():
    data: list[list[str]] = []
    count = 0
    for line in open("input.txt").readlines():
        data.append(list(line))

    for i in range(len(data) - 1):
        if i == 0:
            continue
        for j in range(len(data[i]) - 1):
            if j == 0:
                continue
            v = data[i][j]
            if v == "A":
                count += 1 if testIndex(i, j, data) else 0

    print(count)


main()
