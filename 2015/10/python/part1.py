file = open("input.txt", "r")
line = file.read().strip()


def get_next(s):
    ret = ""
    count = 0
    current = s[0]

    for c in s:
        if c == current:
            count += 1
        else:
            ret += f"{count}{current}"
            count = 1
            current = c

    ret += f"{count}{current}"
    return ret


for _ in range(50):
    line = get_next(line)

print(len(line))
