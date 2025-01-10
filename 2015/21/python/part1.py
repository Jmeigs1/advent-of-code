import re

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
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3
"""


def does_player_win(player_stats, mob_stats):
    ph, pd, pa = player_stats
    mh, md, ma = mob_stats

    # There is a bug here that didn't affect p1.  Fixed in p2
    m_turns = mh // max(1, pd - ma)
    p_turns = ph // max(1, md - pa)

    return p_turns >= m_turns


def get_nums(s: str):
    matches = re.finditer(r"\d+", s)
    return list(map(lambda m: int(m.group(0)), list(matches)))


file = open("input.txt", "r")
info = get_nums(file.read().strip())

winners = []
for i in range(10):
    for j in range(10):
        if does_player_win((100, i, j), tuple(info)):
            winners.append((i, j))
            break

for win in winners:
    print(win)
