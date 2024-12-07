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
    workingList = [nums]
    expectedSize = len(funcs) ** (len(nums) - 1)

    while True:
        if len(workingList) == expectedSize:
            return any(list(map(lambda x: x[0] == final, workingList)))
        else:
            newList = []
            for item in workingList:
                for func in funcs:
                    newNumber = func(item[0], item[1])
                    newList.append([newNumber] + item[2:])
            workingList = newList


main()
