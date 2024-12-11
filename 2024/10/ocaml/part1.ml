type cell = int * int
type lines = string list
type grid_array = string array

let _split_on_whitespace s = s |> Str.split (Str.regexp "[ ]+")

let findStarts (lines : lines) : cell list * grid_array =
  let ibound = List.length lines in
  let jbound = String.length (List.hd lines) in
  let arr = Array.init ibound (fun _ -> String.init jbound (fun _ -> 'z')) in

  let rec findStarts i j lst arr gd =
    match (i, j) with
    | i, j when not (i < ibound) -> (lst, arr)
    | i, j when not (j < jbound) ->
        let () = arr.(i) <- List.hd gd in
        findStarts (i + 1) 0 lst arr (List.tl gd)
    | i, j when (List.hd gd).[j] == '0' ->
        findStarts i (j + 1) ((i, j) :: lst) arr gd
    | _ -> findStarts i (j + 1) lst arr gd
  in
  findStarts 0 0 [] arr lines

let isInBounds ibound jbound cell =
  let i, j = cell in
  (not (i < 0)) && (not (j < 0)) && i < ibound && j < jbound

let getAdj ibound jbound start : cell list =
  let isInBounds = isInBounds ibound jbound in
  let dirs = [ (1, 0); (0, 1); (-1, 0); (0, -1) ] in
  let add start dir =
    let i, j = dir in
    let si, sj = start in
    (si + i, sj + j)
  in

  dirs |> List.map (add start) |> List.filter isInBounds

let rec remove_dups lst =
  match lst with
  | [] -> []
  | h :: t -> h :: remove_dups (List.filter (fun x -> x <> h) t)

let findNumberOfTrails (gd : grid_array) start =
  let ibound = Array.length gd in
  let jbound = String.length gd.(0) in

  let isNext h (c : cell) =
    let i, j = c in
    let t = gd.(i).[j] |> Char.escaped |> int_of_string in
    t == h
  in

  let rec bfs height valid =
    match (height, valid) with
    | height, valid when height > 8 -> valid |> remove_dups |> List.length
    | height, valid when List.length valid == 0 -> List.length valid
    | height, valid ->
        let newValid =
          valid
          |> List.map (getAdj ibound jbound)
          |> List.flatten
          |> List.filter (isNext (height + 1))
        in
        bfs (height + 1) newValid
  in
  bfs 0 [ start ]

let () =
  let default = "input.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let lines = s |> String.trim |> String.split_on_char '\n' in

  let starts, gd = lines |> findStarts in

  let trailCounts = starts |> List.map (findNumberOfTrails gd) in
  let count = trailCounts |> List.fold_left ( + ) 0 in

  Printf.printf "%d\n" count;
  close_in ic
