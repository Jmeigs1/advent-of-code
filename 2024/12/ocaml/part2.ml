type grid = string array
type cell = int * int

module CellSet = Set.Make (struct
  type t = cell

  let compare (i, j) (h, k) =
    match (i, j, h, k) with
    | i, j, h, k when i == h -> j - k
    | i, j, h, k -> i - h
end)

let printCell (i, j) = Printf.printf "%d:%d\n" i j
let printCell2 (i, j) = Printf.printf "%d:%d " i j

let addExplore (ap, aa, aseen, aboard) (bp, ba, bseen, bboard) =
  (ap + bp, aa + ba, CellSet.union aseen bseen, CellSet.union aboard bboard)

let isInBounds ibound jbound cell =
  let i, j = cell in
  (not (i < 0)) && (not (j < 0)) && i < ibound && j < jbound

let getAdj start : cell list =
  let dirs = [ (1, 0); (0, 1); (-1, 0); (0, -1) ] in
  let add start dir =
    let i, j = dir in
    let si, sj = start in
    (si + i, sj + j)
  in
  dirs |> List.map (add start)

let exploreRegion gd start =
  let si, sj = start in
  let startVal = gd.(si).[sj] in

  let ibound = Array.length gd in
  let jbound = String.length gd.(0) in
  let isInBounds = isInBounds ibound jbound in

  let seen = CellSet.empty in
  let borders = CellSet.empty in

  let rec explore seen borders current =
    let newA = 1 in
    let adj = getAdj current in
    let newSeen = CellSet.add current seen in

    let validAdj =
      adj |> List.filter isInBounds
      |> List.filter (fun (i, j) -> gd.(i).[j] == startVal)
    in

    let invalidAdj =
      adj
      |> List.filter (fun ((i, j) as x) ->
             (not (isInBounds x)) || gd.(i).[j] != startVal)
      |> List.filter (fun (i, j) ->
             let ci, cj = current in
             let di = (i - ci) * i in
             let dj = (j - cj) * j in
             (* Printf.printf "%d:%d " di dj; *)
             not (CellSet.mem (di, dj) borders))
    in

    let newBorders =
      invalidAdj
      |> List.fold_left
           (fun b (i, j) ->
             let ci, cj = current in
             let di = (i - ci) * i in
             let dj = (j - cj) * j in

             CellSet.add (di, dj) b)
           borders
    in

    let newP = List.length invalidAdj in

    (* Printf.printf "%d - %d\n" newP newA; *)
    let validAdjUnseen =
      validAdj |> List.filter (fun x -> not (CellSet.mem x seen))
    in

    let value =
      validAdjUnseen
      |> List.fold_left
           (fun (ap, aa, aseen, aboard) (ai, aj) ->
             let ret =
               if CellSet.mem (ai, aj) aseen then (0, 0, aseen, aboard)
               else explore aseen aboard (ai, aj)
             in

             addExplore (ap, aa, aseen, aboard) ret)
           (newP, newA, newSeen, newBorders)
    in

    value
  in

  let p, a, seen, borders = explore seen borders start in
  (* let () = CellSet.iter (fun (i, j) -> Printf.printf "%d|%d\n" i j) borders
     in *)
  (p, a, seen)

let exploreGrid gd =
  let ibound = Array.length gd in
  let jbound = String.length gd.(0) in

  let seen = CellSet.empty in

  let rec explore i j count seen =
    match (i, j) with
    | i, j when not (i < ibound) -> count
    | i, j when not (j < jbound) -> explore (i + 1) 0 count seen
    | i, j when not (CellSet.mem (i, j) seen) ->
        let p, a, nseen = exploreRegion gd (i, j) in
        explore i (j + 1) (count + (p * a)) (CellSet.union nseen seen)
    | _ -> explore i (j + 1) count seen
  in

  explore 0 0 0 seen

let () =
  let default = "input2.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let lines = s |> String.trim |> String.split_on_char '\n' in
  let gd = lines |> Array.of_list in

  gd |> exploreGrid |> string_of_int |> print_endline;

  close_in ic
