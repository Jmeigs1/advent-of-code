from itertools import product


def main():
    count = 0

    with open("input.txt", "r") as file:
        for d in file:
            line = d.strip()
            final, parts = line.split(":")

            b = evaluateLine(final, parts)
            if b:
                count += int(final)

    print(count)


def add(a, b):
    return a + b


def mult(a, b):
    return a * b


def concat(a, b):
    return int(f"{a}{b}")


def evaluateLine(final: str, parts: str):
    nums = list(map(int, parts.strip().split(" ")))

    funcs = [add, mult, concat]

    opts = product(range(len(funcs)), repeat=(len(nums)))

    for o in opts:
        count = nums[0]
        for n in range(len(nums) - 1):
            funcSelect = o[n]
            count = funcs[funcSelect](count, nums[n + 1])
        if count == int(final):
            return True
    return False


main()
