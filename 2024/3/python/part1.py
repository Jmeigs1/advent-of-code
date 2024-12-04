import re

count = 0

with open("input.txt", "r") as file:
    content = file.read()
    regex = "mul\\((\\d+),(\\d+)\\)"
    # print(regex)
    matches = re.findall(regex, content)
    if matches is None:
        exit(1)
    for match in matches:
        count += int(match[0]) * int(match[1])

print(count)
