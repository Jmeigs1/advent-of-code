import re


red_total = 12
green_total = 13
blue_total = 14


def is_valid_pull(data_str: str) -> bool:
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

            if color == "red" and num > red_total:
                return False
            elif color == "green" and num > green_total:
                return False
            elif color == "blue" and num > blue_total:
                return False
    return True


def main():
    count = 0
    for line in open("input.txt"):
        id_data, data = line.split(":")
        id = re.search(r"\d+", id_data)
        if id is None:
            raise ValueError(f"Invalid ID data: {id_data}")
        id = id.group(0)

        if is_valid_pull(data):
            count += int(id)

    print(count)


main()
