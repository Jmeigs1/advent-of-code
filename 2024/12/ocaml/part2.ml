type grid = string array
type cell = int * int

module CellSet = Set.Make (struct
  type t = cell

  let compare (i, j) (h, k) =
    match (i, j, h, k) with
    | i, j, h, k when i == h -> j - k
    | i, j, h, k -> i - h
end)

module CellMap = Map.Make (String)

let printCell (i, j) = Printf.printf "%d:%d\n" i j
let printCell2 (i, j) = Printf.printf "%d:%d " i j

let addExplore (ap, aa, aseen, aboard) (bp, ba, bseen, bboard) =
  let pick key v1 v2 = Some (Int.max v1 v2) in
  (ap + bp, aa + ba, CellSet.union aseen bseen, CellMap.union pick aboard bboard)

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

let tupToString (i, j) = Printf.sprintf "%d:%d" i j

let exploreRegion gd start =
  let fromGrid (i, j) = gd.(i).[j] in
  let startVal = fromGrid start in
  let isStartVal (i, j) = gd.(i).[j] == startVal in

  let ibound = Array.length gd in
  let jbound = String.length gd.(0) in
  let isInBounds = isInBounds ibound jbound in

  let seen = CellSet.empty in
  let borders = CellMap.empty in

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
    in

    let invalidAdjBorderSeen =
      invalidAdj |> List.filter (fun x -> CellMap.mem (tupToString x) borders)
    in

    let newBorders =
      invalidAdj |> List.filter isInBounds
      |> List.fold_left
           (fun b ((i, j) as x) ->
             CellMap.update (tupToString x)
               (fun v -> match v with None -> Some 1 | Some v -> Some (v + 1))
               b)
           borders
    in

    let rec remove_dups lst =
      match lst with
      | [] -> []
      | ((i, j) as h) :: t ->
          h
          :: remove_dups
               (List.filter (fun (xi, xj) -> not (xi == i || xj == j)) t)
    in

    let externalAngles =
      match List.length invalidAdj with
      | 4 -> 4
      | 3 -> 2
      | 2 -> List.length (invalidAdj |> remove_dups) - 1
      | 1 -> 0
      | 0 -> 0
      | _ -> failwith "bad ex angles"
    in

    (* there is a nasty bug here. Can over count if groups with same letter are
       too close - See bad-input *)
    let findInternalAngles (i, j) =
      let baseCorner = [ (1, 0); (1, 1); (0, 1) ] in
      let dirs = [ (1, 1); (1, -1); (-1, 1); (-1, -1) ] in

      let combos =
        dirs
        |> List.map (fun (di, dj) ->
               List.map (fun (bi, bj) -> (di * bi, dj * bj)) baseCorner)
      in

      let v =
        combos
        |> List.filter
             (List.for_all (fun (ti, tj) ->
                  let v = (i + ti, j + tj) in
                  isInBounds v && isStartVal v))
        |> List.length
      in
      v
    in

    let internalAngles =
      invalidAdjBorderSeen
      |> List.map (fun v ->
             let vl = CellMap.find (tupToString v) borders in
             match vl with
             | 3 -> 0
             | 2 -> 0
             | 1 -> findInternalAngles v
             | x ->
                 failwith
                   (Printf.sprintf "bad in angle %c %d current:%s looking:%s"
                      startVal x (tupToString current) (tupToString v)))
      |> List.fold_left ( + ) 0
    in

    let newP = externalAngles + internalAngles in

    (* Printf.printf "newp %d\n" newP; *)
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
  let default = "input.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let lines = s |> String.trim |> String.split_on_char '\n' in
  let gd = lines |> Array.of_list in

  gd |> exploreGrid |> string_of_int |> print_endline;

  close_in ic
