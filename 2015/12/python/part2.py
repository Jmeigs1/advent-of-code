from json import loads

file = open("input.txt", "r")
line = file.read().strip()

data = loads(line)


def dig(i):
    count = 0
    if isinstance(i, dict):
        if "red" not in i.values():
            for k in i.values():
                count += dig(k)
    elif isinstance(i, list):
        for k in i:
            count += dig(k)
    elif isinstance(i, int):
        return i

    return count


print(dig(data))
