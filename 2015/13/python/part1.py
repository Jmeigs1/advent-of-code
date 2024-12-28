import itertools

graph = {}
people = set("me")


def get_happiness(people):
    count = 0
    size = len(people)
    for i in range(size):
        main = people[i]
        j = (i - 1) % size
        left = people[j]
        k = (i + 1) % size
        right = people[k]

        count += graph[(main, left)]
        count += graph[(main, right)]
    return count


file = open("input.txt", "r")
for line in file:
    words = line.strip()[0:-1].split(" ")

    p1 = words[0]
    p2 = words[-1]
    dir = 1 if words[2] == "gain" else -1
    ammount = int(words[3])
    graph[(p1, p2)] = ammount * dir
    people.add(p1)
    people.add(p2)

perms = itertools.permutations(people)

max_h = 0

for ppl in perms:
    h = get_happiness(ppl)
    max_h = max(max_h, h)

print(max_h)
