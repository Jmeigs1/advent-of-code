with open("input.txt", "r") as file:
    content = file.read().strip()

    up = content.count("(")
    down = content.count(")")
    print(up - down)
