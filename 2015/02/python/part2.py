def get_size(l, w, h):
    return 2 * l * w + 2 * w * h + 2 * h * l + min(l * w, w * h, h * l)


def get_ribbon_len(l, w, h):
    return 2 * ((l + w + h) - max(l, w, h)) + l * w * h


count = 0
file = open("input.txt", "r")
for line in file:
    l, w, h = map(int, line.split("x"))
    count += get_ribbon_len(l, w, h)

print(count)
