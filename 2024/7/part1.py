def main():
    count = 0
    with open("input.txt", "r") as file:
        for d in file:
            line = d.strip()
            final, parts = line.split(":")
            count += int(final) if evaluateLine(final, parts) else 0
    print(count)


def add(a, b):
    return a + b


def mult(a, b):
    return a * b


def evaluateLine(final: str, parts: str):
    nums = list(map(int, parts.strip().split(" ")))
    size = 2 ** (len(nums) - 1)

    funcs = [add, mult]

    for i in range(size):
        count = nums[0]
        for n in range(len(nums) - 1):
            funcSelect = (i >> n) & 1
            count = funcs[funcSelect](count, nums[n + 1])
        if count == int(final):
            return True
    return False


main()
