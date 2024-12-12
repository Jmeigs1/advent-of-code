type grid = string array
type cell = int * int

module CellSet = Set.Make (struct
  type t = cell

  let compare (i, j) (h, k) =
    match (i, j, h, k) with
    | i, j, h, k when i == h -> j - k
    | i, j, h, k -> i - h
end)

let findStart gd =
  let ibound = Array.length gd in
  let jbound = String.length gd.(0) in

  let rec find i j =
    match (i, j) with
    | i, j when i == ibound -> failwith "bad find"
    | i, j when j == jbound -> find (i + 1) 0
    | i, j when gd.(i).[j] == '^' -> (i, j)
    | i, j -> find i (j + 1)
  in

  find 0 0

let isInBounds ibound jbound cell =
  let i, j = cell in
  (not (i < 0)) && (not (j < 0)) && i < ibound && j < jbound

let walkAndCount gd start =
  let dirs = [| (-1, 0); (0, 1); (1, 0); (0, -1) |] in
  let nextDir i = (i + 1) mod 4 in

  let ibound = Array.length gd in
  let jbound = String.length gd.(0) in

  let rec walk seen dir current =
    let di, dj = dirs.(dir) in
    let ci, cj = current in

    let next = (ci + di, cj + dj) in
    let newSeen = CellSet.add current seen in

    if isInBounds ibound jbound next then
      let ni, nj = next in
      if gd.(ni).[nj] == '#' then walk newSeen (nextDir dir) current
      else walk newSeen dir next
    else CellSet.cardinal newSeen
  in

  walk CellSet.empty 0 start

let () =
  let default = "input.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let lines = s |> String.trim |> String.split_on_char '\n' in
  let gd = Array.of_list lines in

  let count = gd |> findStart |> walkAndCount gd in

  count |> Printf.printf "%d\n";

  close_in ic
