let default = "input.txt" in
let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
let ic = open_in file in

let _test = Str.regexp in

let s = really_input_string ic (in_channel_length ic) in

let split_on_whitespace s =
  s |> String.split_on_char ' ' |> List.filter (fun s -> String.length s != 0)
in

let lists =
  s |> String.trim |> String.split_on_char '\n' |> List.map split_on_whitespace
in

let prepList (l : string list list) i =
  l
  |> List.map (fun idx -> List.nth idx i)
  |> List.map int_of_string
  |> List.sort (fun a b -> a - b)
in

let abs_diff a b = a - b |> Int.abs in

let rec getCount l1 l2 i count =
  if i == -1 then count
  else
    let newValue = count + abs_diff (List.nth l1 i) (List.nth l2 i) in
    getCount l1 l2 (i - 1) newValue
in

let list1 = prepList lists 0 in
let list2 = prepList lists 1 in

getCount list1 list2 (List.length list1 - 1) 0 |> string_of_int |> print_endline;
close_in ic
