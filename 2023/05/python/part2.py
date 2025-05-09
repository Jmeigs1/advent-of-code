def pairwise(t):
    it = iter(t)
    return zip(it, it)


def eval_seed_maps(target, depth, seed_maps):
    if len(seed_maps) == 0:
        return target
    else:
        current_map = seed_maps[-1]
        rest = seed_maps[:-1]
        new_target = target

        for part in current_map:
            start_source, start_target, remap_range = part

            if start_target <= target <= start_target + remap_range:
                new_target = start_source + (target - start_target)
                break

        end_value = eval_seed_maps(new_target, depth + 1, rest)
        return end_value


def check_solution(target, seed_pairs):
    for pair in seed_pairs:
        if pair[0] <= target <= pair[0] + pair[1]:
            return True

    return False


def main():
    data = open("input.txt", "r").read()

    parts = data.split("\n\n")

    first_part = parts[0]
    seeds = first_part.split(":")[1].strip().split(" ")
    seeds = map(int, seeds)
    seed_pairs = list(pairwise(seeds))

    seed_maps = []

    for part in parts[1:]:
        part = part.strip()
        seed_map = []
        lines = part.split("\n")[1:]
        for line in lines:
            target, source, remap_range = line.split(" ")

            start_target = int(target)
            start_source = int(source)
            remap_range = int(remap_range)

            seed_map.append((start_source, start_target, remap_range))

        seed_maps.append(seed_map)

    i = 0
    limit_top = 100_000_000

    for i in range(0, limit_top, 1000):
        seed_from_target = eval_seed_maps(i, 0, seed_maps)

        valid = check_solution(seed_from_target, seed_pairs)
        if valid:
            for j in range(i - 1000, limit_top, 1):
                seed_from_target = eval_seed_maps(j, 0, seed_maps)

                if check_solution(seed_from_target, seed_pairs):
                    print("Found solution:", j, "->", seed_from_target)
                    return
            return

    print("No solution found")


main()
