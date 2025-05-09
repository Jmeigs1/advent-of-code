def main():
    cache = {}
    data = open("input.txt", "r").read()

    parts = data.split("\n\n")

    first_part = parts[0]
    seeds = first_part.split(":")[1].strip().split(" ")
    seeds = map(int, seeds)

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

    minimum = float("inf")
    for seed in seeds:
        if seed in cache:
            seed = cache[seed]
        else:
            start = seed
            for seed_map in seed_maps:
                for part in seed_map:
                    start_source, start_target, remap_range = part

                    if start_source <= seed <= start_source + remap_range:
                        new_seed = start_target + (seed - start_source)
                        # print(seed, "->", new_seed)
                        seed = new_seed
                        break
            cache[start] = seed

        minimum = min(minimum, seed)

    print(minimum)


main()
