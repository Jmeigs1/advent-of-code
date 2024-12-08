let parseRules rules =
  rules |> String.trim |> String.split_on_char '\n'
  |> List.map (fun lst -> String.split_on_char '|' lst)
  |> List.map (fun lst -> List.map (fun x -> int_of_string x) lst)

let parseBooks books =
  books |> String.trim |> String.split_on_char '\n'
  |> List.map (fun lst -> String.split_on_char ',' lst)
  |> List.map (fun lst -> List.map (fun x -> int_of_string x) lst)

let isBadRule (book : int list) (rule : int list) =
  let find p = List.find_index (fun page -> page == p) book in
  let rule1 = List.nth rule 0 in
  let rule2 = List.nth rule 1 in
  let f1 = find rule1 in
  let f2 = find rule2 in
  f1 != None && f2 != None && f1 > f2

let isGoodBook (book : int list) (rules : int list list) =
  rules |> List.find_index (fun r -> isBadRule book r) |> fun x ->
  match x with None -> true | _n -> false

let getMiddle list = list |> List.map (fun x -> List.nth x (List.length x / 2))

let () =
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else "input.txt" in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let split = Str.regexp "\n\n" |> Str.split in
  let parts = s |> split in

  let rules = parseRules (List.nth parts 0) in
  let books = List.nth parts 1 |> parseBooks in

  let count =
    books
    |> List.filter (fun b -> isGoodBook b rules)
    |> getMiddle |> Helpers.sum
  in

  print_endline (string_of_int count);
  close_in ic
