with open("input.txt", "r") as file:
    content = file.read().strip()
    count = 0
    i = 1

    for c in content:
        count += 1 if c == "(" else -1
        if count == -1:
            print(i)
            break
        i += 1
