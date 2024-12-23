def has_3_vowels(s):
    vowels = "aeiou"
    count = 0
    for c in s:
        if vowels.find(c) != -1:
            count += 1
            if count == 3:
                return True
    return False


def has_repeat(s):
    last = ""
    for c in s:
        if c == last:
            return True
        last = c
    return False


def has_bad_string(s: str):
    bad_strings = ["ab", "cd", "pq", "xy"]
    last = ""
    for c in s:
        if (last + c) in bad_strings:
            return True
        last = c
    return False


def is_good_str(s):
    return has_3_vowels(s) and has_repeat(s) and not has_bad_string(s)


file = open("input.txt", "r")

count = 0
for line in file:
    if is_good_str(line):
        count += 1

print(count)
