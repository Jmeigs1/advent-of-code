import re


def replace(full, s1, s2, pos):
    return full[0:pos] + s2 + full[pos + len(s1) :]


replacements = {}
replacements2 = {}
results = set()

file = open("input.txt", "r")
p1, end_string = file.read().strip().split("\n\n")

for line in p1.splitlines():
    i, o = line.split(" => ")
    replacements[i] = o
    replacements2[o] = i

count = 0
while len(end_string) > 1:
    found = False
    for key in replacements2:
        val = replacements2[key]
        match = re.search(key, end_string)

        if match is not None:
            found = True
            end_string = replace(end_string, key, val, match.start())
            count += 1
            break

    if not found:
        break

print(count)
