let cache = Hashtbl.create 100000000

let getSize num =
  let rec getSize i num =
    let n = num / 10 in
    if n == 0 then i else getSize (i + 1) n
  in
  getSize 1 num

let splitStr str =
  let len = String.length str in
  let mid = Int.shift_right len 1 in
  (String.sub str 0 mid, String.sub str mid (len - mid))

let splitInt i = i |> string_of_int |> splitStr
let canSplitInt i = i != 0 && (i |> getSize) mod 2 == 0

let applyRules n i =
  let rec applyRules n i =
    if n == 0 then 1
    else
      let key = (i, n) in
      let found = Hashtbl.find_opt cache key in
      if found != None then Option.get found
      else
        let value =
          match i with
          | i when i == 0 -> applyRules (n - 1) 1
          | i when canSplitInt i ->
              let left, right = splitInt i in
              applyRules (n - 1) (int_of_string left)
              + applyRules (n - 1) (int_of_string right)
          | i -> applyRules (n - 1) (i * 2024)
        in
        let () = Hashtbl.add cache key value in
        value
  in
  applyRules n i

let rec loop i fn lst = if i == 0 then lst else loop (i - 1) fn (fn lst)

let () =
  let default = "input.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let start =
    s |> String.trim |> String.split_on_char ' ' |> List.map int_of_string
  in

  let fin = start |> List.map (applyRules 75) |> List.fold_left ( + ) 0 in

  (* let test = 512072 in *)
  (* let () = test |> canSplitInt |> Printf.printf "%b\n" in *)
  (* let () = test |> splitInt |> fun (i, j) -> Printf.printf "%s:%s\n" i j in *)
  (* fin |> List.iter (Printf.printf "%d\n"); *)
  fin |> Printf.printf "%d\n";
  close_in ic
