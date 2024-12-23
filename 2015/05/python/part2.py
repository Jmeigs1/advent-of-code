def has_repeat_one_sep(s):
    last = ""
    last2 = ""

    for c in s:
        print(last2, last, c)
        if c == last2:
            return True
        last2 = last
        last = c
    return False


def has_pair(s: str):
    for i in range(len(s) - 2):
        key = s[i] + s[i + 1]
        if s.find(key, i + 2) != -1:
            return True
    return False


def is_good_str(s):
    return has_pair(s) and has_repeat_one_sep(s)


file = open("input.txt", "r")

count = 0
for line in file:
    if is_good_str(line):
        count += 1
print(count)
