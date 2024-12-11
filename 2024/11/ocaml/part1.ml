let splitStr str =
  let len = String.length str in
  let mid = len / 2 in
  [ String.sub str 0 mid; String.sub str mid (len - mid) ]

let splitInt i = i |> string_of_int |> splitStr |> List.map int_of_string
let canSplitInt i = i != 0 && (i |> string_of_int |> String.length) mod 2 == 0

let applyRule i =
  match i with
  | i when i == 0 -> [ 1 ]
  | i when canSplitInt i -> splitInt i
  | i -> [ i * 2024 ]

let rec loop i fn lst = if i == 0 then lst else loop (i - 1) fn (fn lst)

let () =
  let default = "input.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let start =
    s |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
  in

  let fin = loop 25 (fun i -> List.map applyRule i |> List.flatten) start in

  (* fin |> List.iter (Printf.printf "%d\n"); *)
  fin |> List.length |> Printf.printf "%d\n";
  close_in ic
