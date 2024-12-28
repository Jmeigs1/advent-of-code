from collections import defaultdict
import re


def nums(s):
    return re.findall(r"\d+", s)


def get_dist(speed, go_time, rest_time, t):
    base = go_time + rest_time
    dist = (t // base) * go_time * speed + min((t % base), go_time) * speed
    return dist


deer = []
scores = defaultdict(int)

file = open("input.txt", "r")
for line in file:
    line = line.strip()
    (speed, go_time, rest_time) = tuple(map(int, nums(line)))
    deer.append((speed, go_time, rest_time))

for t in range(1, 2503):
    bestList = []
    bestScore = 0
    for i, d in enumerate(deer):
        (speed, go_time, rest_time) = d
        dist = get_dist(speed, go_time, rest_time, t)
        if dist > bestScore:
            bestList = [i]
            bestScore = dist
        elif dist == bestScore:
            bestList.append(i)

    for idx in bestList:
        scores[idx] += 1

print(max(scores.values()))
