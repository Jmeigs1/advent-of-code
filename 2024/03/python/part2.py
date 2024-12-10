import re

count = 0
regex = "mul\\((\\d+),(\\d+)\\)"

doString = "do()"
dontString = "don't()"


def matchesString(curr: str, i: int, target: str):
    size = len(target)
    substring = curr[i : i + size]
    # print(substring, target, substring == target)
    return substring == target


def parse(input: str):
    retString = ""
    enabled = True

    for i in range(len(input)):
        if input[i] == "d":
            if not enabled and matchesString(input, i, doString):
                enabled = True
            elif enabled and matchesString(input, i, dontString):
                enabled = False
        if enabled:
            retString += input[i]

    return retString


with open("input.txt", "r") as file:
    data = file.read()
    content = parse(data)
    # print(content)
    matches = re.findall(regex, content)
    if matches is None:
        exit(1)
    for match in matches:
        count += int(match[0]) * int(match[1])

print(count)
