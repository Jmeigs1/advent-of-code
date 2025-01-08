import math


def find_factors(n: int):
    factors = []
    end = int(math.sqrt(n))
    for i in range(1, end):
        if n % i == 0:
            if n / i == i:
                factors.append(i)
            else:
                factors.append(i)
                factors.append(n // i)

    return factors


file = open("input.txt", "r")
input = int(file.read())

found = False
i = 0

while not found:
    i += 1
    if sum(find_factors(i)) * 10 >= input:
        print(i)
        found = True
