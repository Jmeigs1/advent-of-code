def get_game_power(data_str: str) -> int:
    red_game = 0
    blue_game = 0
    green_game = 0

    pulls = data_str.strip().split(";")
    for pull in pulls:
        pull_str = pull.strip()
        counts = pull_str.split(",")
        if len(counts) > 3:
            raise ValueError("More than 3 colors found")

        for count in counts:
            count = count.strip()
            if count == "":
                raise ValueError("Empty count found")
            num, color = count.split(" ")
            num = int(num.strip())
            color = color.strip()

            if color == "red":
                red_game = max(red_game, num)
            elif color == "green":
                green_game = max(green_game, num)
            elif color == "blue":
                blue_game = max(blue_game, num)

    if red_game == 0 or green_game == 0 or blue_game == 0:
        raise Exception("Game power is 0")

    return red_game * green_game * blue_game


def main():
    count = 0
    for line in open("input.txt"):
        _, data = line.split(":")

        count += get_game_power(data)

    print(count)


main()
