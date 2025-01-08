import math


def find_factors(n: int):
    factors = []
    end = int(math.sqrt(n))
    for i in range(1, end):
        if n % i == 0:
            if n // i == i:
                if n <= i * 50:
                    factors.append(i)
            else:
                if n <= i * 50:
                    factors.append(i)
                if n <= (n // i) * 50:
                    factors.append(n // i)

    return factors


file = open("input.txt", "r")
input = int(file.read())

found = False
i = 0

while not found:
    i += 1
    res = sum(find_factors(i)) * 11
    if res >= input:
        print(i)
        found = True
