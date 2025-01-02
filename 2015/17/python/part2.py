import itertools

size = 150

cups = []
file = open("input.txt", "r")
for line in file:
    line = line.strip()
    line = int(line)
    cups.append(line)

count = 0
found = False

for i in range(1, len(cups) - 1):
    for x in itertools.combinations(cups, i):
        if sum(x) == 150:
            found = True
            count += 1
    if found:
        break


print(count)
