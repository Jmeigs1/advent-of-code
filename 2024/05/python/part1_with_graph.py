from collections import defaultdict
import sys

sys.setrecursionlimit(10**7)


def dfs(
    start: int,
    idx: int,
    lst: list[int],
    allowed: set[int],
    edges: defaultdict[int, list[int]],
):
    if lst.index(start) < idx:
        return False

    children = edges[start]
    children = list(filter((lambda x: x in allowed), children))

    if len(children) == 0:
        return True

    for c in children:
        if not dfs(c, idx, lst, allowed, edges):
            return False

    return True


def main():
    count = 0

    file = open("input.txt", "r")
    part1, part2 = file.read().strip().split("\n\n")

    edges = part1.strip().split("\n")
    edges = [e.split("|") for e in edges]
    edges = [(int(e[0]), int(e[1])) for e in edges]
    edgesDict = defaultdict(list)
    for e in edges:
        edgesDict[e[0]].append(e[1])

    pageLists = part2.strip().split("\n")
    pageLists = [p.split(",") for p in pageLists]
    pageLists = [[int(d) for d in p] for p in pageLists]

    for lst in pageLists:
        allowed = set(lst)
        good = True

        for i in range(len(lst) - 1, 0, -1):
            p = lst[i]
            if not dfs(p, i, lst, allowed, edgesDict):
                good = False
                break
            allowed.remove(p)

        if good:
            count += lst[len(lst) >> 1]

    file.close()
    print(count)


main()
