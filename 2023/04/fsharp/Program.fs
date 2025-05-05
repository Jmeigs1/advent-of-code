open Shared

let part1 =
    let data = read_lines "input.txt"
    data |> Seq.map check_line_points |> Seq.sum |> printfn "part1: %d"

let part2 =
    let data = read_lines "input.txt" in
    let counts = data |> Seq.map get_line_count |> Seq.toArray in
    let multipliers = Array.create (Array.length counts) (bigint 1) in
    let mutable totalCount: bigint = bigint 0 in

    for i in 0 .. Array.length counts - 1 do
        totalCount <- totalCount + multipliers[i]

        for j in i + 1 .. (min (Array.length counts - 1) (i + counts[i])) do
            multipliers[j] <- multipliers[j] + multipliers[i]

    printfn "part2: %A" totalCount


[<EntryPoint>]
let main argv =
    part1
    part2
    0
