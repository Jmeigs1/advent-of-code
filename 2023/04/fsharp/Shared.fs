module Shared

let read_lines filePath = System.IO.File.ReadLines filePath

let split_string (delimiter: string) (input: string) = input.Split(delimiter)

let removeUpToChar (input: string) (charToSplit: char) =
    let parts = input.Split([| charToSplit |])

    if parts.Length > 1 then
        String.concat "" (parts |> Seq.skip 1)
    else
        input

let get_points (count: int) =
    if count = 0 then 0 else pown 2 (count - 1)

let get_line_count (line: string) =
    let parts = (removeUpToChar line ':') |> split_string " | " in

    if parts.Length <> 2 then
        failwith "Invalid line format"

    let winners =
        parts[0].Split(" ") |> Array.filter (fun x -> x.Length <> 0) |> Array.map int in

    let card_numbers =
        parts[1].Split(" ") |> Array.filter (fun x -> x.Length <> 0) |> Array.map int in

    let count =
        card_numbers |> Array.filter (fun x -> Array.contains x winners) |> Array.length in

    count

let check_line_points (line: string) = get_line_count line |> get_points
