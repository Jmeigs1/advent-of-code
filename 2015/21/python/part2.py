import re
import math


def get_nums(s: str):
    matches = re.finditer(r"\d+", s)
    return list(map(lambda m: int(m.group(0)), list(matches)))


items = """
Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage     25     1       0
Damage     50     2       0
Damage    100     3       0
Defense    20     0       1
Defense    40     0       2
Defense    80     0       3
"""

parts = items.strip().split("\n\n")

shop = []

first = True

for p in parts:
    items = []
    if not first:
        items.append(tuple([0, 0, 0]))
    first = False
    lines = p.split("\n")[1:]
    for line in lines:
        items.append(tuple(get_nums(line)))
    shop.append(items)

weapons, armor, rings = shop


def does_player_win(player_stats, mob_stats):
    ph, pd, pa = player_stats
    mh, md, ma = mob_stats

    m_turns = (mh + max(1, pd - ma) - 1) // max(1, pd - ma)
    p_turns = (ph + max(1, md - pa) - 1) // max(1, md - pa)

    return p_turns >= m_turns


file = open("input.txt", "r")
mob_info = get_nums(file.read().strip())

most_cost = 0

for w in weapons:
    for a in armor:
        for lr in rings:
            for rr in rings:
                if lr == rr:
                    continue
                cost, dmg, df = map(sum, zip(w, a, lr, rr))
                if not does_player_win((100, dmg, df), tuple(mob_info)):
                    if cost > most_cost:
                        most_cost = cost

print(most_cost)
