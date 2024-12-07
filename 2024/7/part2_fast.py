def main():
    count = 0

    with open("input.txt", "r") as file:
        for d in file:
            line = d.strip()
            final, parts = line.split(":")
            nums = list(map(int, parts.strip().split(" ")))

            ev = evaluateLine(int(final), nums)
            if ev:
                count += int(final)

    print(count)


def add(a, b):
    return a + b


def mult(a, b):
    return a * b


def concat(a, b):
    return int(f"{a}{b}")


funcs = [add, mult, concat]


def evaluateLine(final: int, nums: list[int]):
    if len(nums) == 1:
        return final == nums[0]

    for func in funcs:
        newNumber = func(nums[0], nums[1])
        if newNumber > final:
            continue

        ev = evaluateLine(final, [newNumber] + nums[2:])
        if ev:
            return True
    return False


main()
