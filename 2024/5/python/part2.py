import re
from typing import Any

isPair = "^(\\d{2})\\|(\\d{2})$"
count = 0

checkMap: dict[int, list[int]] = {}


def fixLine(
    line: str,
    checkMap: dict[int, list[int]],
):
    # value - index
    seen: dict[int, int] = {}
    splitLine = [int(sln) for sln in line.split(",")]
    i = 0
    while i < len(splitLine):
        print(i)
        n = splitLine[i]
        worst = None
        for second in checkMap[n]:
            if second in seen:
                if worst is None:
                    worst = seen[second]
                else:
                    worst = min(worst, seen[second])
        if worst is not None:
            swapIndex = worst
            print("swapping", line, "|", i, swapIndex)
            splitLine[i] = splitLine[swapIndex]
            # Can do better than rechecking everything here
            splitLine[swapIndex] = n
            i = 0
            seen = {}
        seen[n] = i
        i += 1
    return splitLine


def evalLine(
    line: str,
    checkMap: dict[int, list[int]],
):
    seen: set[int] = set()
    splitLine = [int(sln) for sln in line.split(",")]
    for n in splitLine:
        for second in checkMap[n]:
            if second in seen:
                return None
        seen.add(n)
    return splitLine[len(splitLine) >> 1]


def addValue(map: dict[int, list[int]], key: int, value: int):
    if key not in map:
        map[key] = [value]
        return

    map[key].append(value)


def getMiddle(myList: list[Any]):
    return myList[len(myList) >> 1]


with open("input.txt", "r") as file:
    i = 0
    for line in [fullLine.rstrip() for fullLine in file]:
        if line == "":
            continue
        match = re.match(isPair, line)
        if match is not None:
            addValue(checkMap, int(match.group(1)), int(match.group(2)))
        elif evalLine(line, checkMap) is None:
            i += 1
            if i == 1 or True:
                newLine = fixLine(line, checkMap)
                newLineStr = ",".join([f"{i}" for i in newLine])
                if evalLine(newLineStr, checkMap) is None:
                    print(newLineStr)
                    print(line)
                    print("This shouldn't happen")
                    exit(1)
                count += getMiddle(newLine)

print(count)
