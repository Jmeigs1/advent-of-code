file = open("input.txt", "r")
line = file.read().strip()


def has_pairs(s: str):
    skip = False
    found = False
    pairs = zip(s[:-1], s[1:])

    for p in pairs:
        if skip:
            skip = False
            continue
        a, b = tuple(p)
        if a == b:
            if found:
                return True
            found = True
            skip = True


def string_to_base_26(s):
    res = 0
    s = s[::-1]
    for i, c in enumerate(s):
        res += (ord(c) - 97) * 26**i
    return res


def base_26_to_string(i):
    res = ""
    while i > 0:
        v = i % 26
        res += chr(v + 97)
        i = i // 26

    return res[::-1]


def is_valid(s):
    foundStr8 = False
    last = ""
    last2 = ""

    for c in s:
        if c in ["i", "o", "l"]:
            return False
        if (
            last != ""
            and last2 != ""
            and ord(c) == ord(last) + 1
            and ord(c) == ord(last2) + 2
        ):
            foundStr8 = True
        last2 = last
        last = c

    return foundStr8 and has_pairs(s)


while not is_valid(line):
    enc = string_to_base_26(line)
    line = base_26_to_string(enc + 1)

print(line)
