let _split_on_whitespace s = s |> Str.split (Str.regexp "[ ]+")
let toList s = List.init (String.length s) (fun i -> String.sub s i 1)
let rec dup i j arr = if not (i > 0) then arr else dup (i - 1) j (j :: arr)

let _debugLst lst =
  lst
  |> List.iter (fun x ->
         print_int x;
         print_string " ")

let buildWorkArray lst =
  let open List in
  let rec loopFn i arr =
    if not (i < length lst) then arr
    else
      let value = if i mod 2 == 0 then i / 2 else -1 in
      let newArr = arr @ dup (nth lst i) value [] in
      loopFn (i + 1) newArr
  in
  loopFn 0 []

let processWorkingArray lst =
  let open List in
  let rec loopFn i j count =
    if i > j then count
    else if nth lst i == -1 then
      if nth lst j == -1 then loopFn i (j - 1) count
      else loopFn (i + 1) (j - 1) (count + (nth lst j * i))
    else loopFn (i + 1) j (count + (nth lst i * i))
  in
  loopFn 0 (length lst - 1) 0

let () =
  let default = "input2.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let blocks = s |> String.trim |> toList |> List.map int_of_string in

  let working = blocks |> buildWorkArray in

  working |> processWorkingArray |> string_of_int |> print_endline;

  close_in ic
