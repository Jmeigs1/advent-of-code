import hashlib

file = open("input.txt", "r")
prefix = file.read().strip()


def get_hash(n):
    test = f"{prefix}{n}"
    return hashlib.md5(test.encode()).hexdigest()[0:5]


i = 0
while True:
    if get_hash(i) == "00000":
        print(i)
        break
    i += 1
