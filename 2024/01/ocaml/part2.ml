module IntMap = Map.Make (Int);;

let default = "input.txt" in
let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
let ic = open_in file in

let s = really_input_string ic (in_channel_length ic) in

let split_on_whitespace s = s |> Str.split (Str.regexp "[ ]+") in

let lists =
  s |> String.trim |> String.split_on_char '\n' |> List.map split_on_whitespace
in

let prepList (l : string list list) i =
  l
  |> List.map (fun idx -> List.nth idx i)
  |> List.map int_of_string
  |> List.sort (fun a b -> a - b)
in

let incMapVal mp k =
  let upFunc v = Option.some (Option.value v ~default:0 + 1) in
  IntMap.update k upFunc mp
in

let rec makeMap lst i (mp : int IntMap.t) =
  if i == List.length lst then mp
  else
    let v = List.nth lst i in
    makeMap lst (i + 1) (incMapVal mp v)
in

let rec getCount l1 m2 i count =
  if i == -1 then count
  else
    let v = List.nth l1 i in
    let v2 = IntMap.find_opt v m2 in
    let newCount = count + (v * Option.value v2 ~default:0) in
    getCount l1 m2 (i - 1) newCount
in

let list1 = prepList lists 0 in
let list2 = prepList lists 1 in
let map2 = IntMap.empty |> makeMap list2 0 in

getCount list1 map2 (List.length list1 - 1) 0 |> string_of_int |> print_endline;
close_in ic
