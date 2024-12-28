import json

file = open("input.txt", "r")
count = 0

for line in file:
    line = line.strip()
    encoded = json.dumps(line)
    print(encoded)
    count += len(encoded) - len(line)

print(count)
