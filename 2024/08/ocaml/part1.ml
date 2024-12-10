let _split_on_whitespace s = s |> Str.split (Str.regexp "[ ]+")

let appendAtKey (k : char) (addVal : int list)
    (lst : (char * int list list) list) =
  let oldVal = List.assoc_opt k lst |> Option.value ~default:[] in
  let siteList = oldVal @ [ addVal ] in
  (k, siteList) :: List.remove_assoc k lst

let getAnts grid =
  let rec loopFn i j ants =
    if i > List.length grid - 1 then ants
    else if j > String.length (List.nth grid i) - 1 then loopFn (i + 1) 0 ants
    else
      let tileVal = (List.nth grid i |> String.get) j in
      if tileVal != '.' then
        let tile = [ i; j ] in
        loopFn i (j + 1) (appendAtKey tileVal tile ants)
      else loopFn i (j + 1) ants
  in
  loopFn 0 0 []

let getPairs lst =
  let rec loopFn pairs i j count =
    if not (i < List.length lst) then pairs
    else if not (j < List.length lst) then loopFn pairs (i + 1) (i + 2) count
    else
      let newPair = [ List.nth lst i; List.nth lst j ] in
      loopFn (newPair :: pairs) i (j + 1) (count + 1)
  in
  loopFn [] 0 1 0

let isInBounds boundi boundj i j =
  (not (i < 0 || j < 0)) && i < boundi && j < boundj

let getHotspots isInBounds pair =
  let a1 = List.nth (List.nth pair 0) in
  let a2 = List.nth (List.nth pair 1) in

  let diffi = a1 0 - a2 0 in
  let diffj = a1 1 - a2 1 in

  let site1 = [ a1 0 + diffi; a1 1 + diffj ] in
  let site2 = [ a1 0 - (2 * diffi); a1 1 - (2 * diffj) ] in

  List.filter
    (fun s ->
      let site = List.nth s in
      isInBounds (site 0) (site 1))
    [ site1; site2 ]

let rec remove_duplicates l =
  let eq a b = List.nth a 0 == List.nth b 0 && List.nth a 1 == List.nth b 1 in
  let rec contains l n =
    match l with [] -> false | h :: t -> eq h n || contains t n
  in
  match l with
  | [] -> []
  | h :: t ->
      let acc = remove_duplicates t in
      if contains acc h then acc else h :: acc

let () =
  let default = "input.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let grid = s |> String.trim |> String.split_on_char '\n' in
  let ants = getAnts grid in

  let keys = List.map fst ants in
  let pairs =
    keys |> List.fold_left (fun a c -> a @ getPairs (List.assoc c ants)) []
  in

  let isInBounds =
    isInBounds (List.length grid) (String.length (List.nth grid 0))
  in

  let hsps =
    pairs |> List.fold_left (fun a p -> a @ getHotspots isInBounds p) []
  in

  hsps |> remove_duplicates |> List.length |> string_of_int |> print_endline;
  close_in ic
