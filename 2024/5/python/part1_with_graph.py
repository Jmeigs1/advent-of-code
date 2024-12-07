from collections import defaultdict
import sys

sys.setrecursionlimit(10**7)

depthChart = {}
seen = set()
count = 0


def dfs(start: int, allowed: set[int], edges: defaultdict[int, list[int]], depth: int):
    global depthChart
    global seen

    if start not in depthChart:
        depthChart[start] = depth
    elif depthChart[start] < depth:
        depthChart[start] = depth

    children = edges[start]
    if start in allowed:
        allowed.remove(start)
    for c in children:
        if c not in allowed:
            continue
        dfs(c, allowed, edges, depth + 1)


def main():
    global count
    global depthChart

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
        depthChart = {}
        allowed = set(lst)
        maxDepth = 0
        good = True

        for p in lst:
            dfs(p, allowed, edgesDict, 0)
        for p in lst:
            if depthChart[p] < maxDepth:
                good = False
                break
            maxDepth = max(maxDepth, depthChart[p])
        if good:
            count += lst[len(lst) >> 1]

    file.close()
    print(count)


main()
