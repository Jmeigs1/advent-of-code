import re

isPair = "^(\\d{2})\\|(\\d{2})$"
count = 0

checkMap: dict[int, list[int]] = {}


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


with open("input.txt", "r") as file:
    for line in [fullLine.rstrip() for fullLine in file]:
        if line == "":
            continue
        match = re.match(isPair, line)
        if match is not None:
            addValue(checkMap, int(match.group(1)), int(match.group(2)))
        else:
            ev = evalLine(line, checkMap)
            if ev is not None:
                count += ev

print(count)
