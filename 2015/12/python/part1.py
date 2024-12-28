import re

file = open("input.txt", "r")
line = file.read().strip()

matches = re.findall(r"-?\d+", line)
count = sum(map(int, matches))
print(count)
