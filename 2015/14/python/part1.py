import re


def nums(s):
    return re.findall(r"\d+", s)


t = 2503
best = 0

file = open("input.txt", "r")
for line in file:
    line = line.strip()
    (speed, go_time, rest_time) = tuple(map(int, nums(line)))
    base = go_time + rest_time
    dist = (t // base) * go_time * speed + min((t % base), go_time) * speed
    best = max(dist, best)

print(best)
