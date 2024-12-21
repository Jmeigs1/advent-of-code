open Core
open Re

type grid = char list list

let split_to_parts s = s |> Str.split (Str.regexp "\n\n")
let get_nums = Re.(alt [ rg '0' '9' ] |> rep1 |> compile)
let get_number_prefix s = Stdlib.String.sub s 0 3 |> Int.of_string

let grid_nums =
  [ [ '7'; '8'; '9' ]; [ '4'; '5'; '6' ]; [ '1'; '2'; '3' ]; [ '_'; '0'; 'A' ] ]

let grid_mvs = [ [ '_'; '^'; 'A' ]; [ '<'; 'v'; '>' ] ]

let grid_to_map (gd : grid) =
  let mp = Map.Poly.empty in

  let rec process_row mp i j row =
    match row with
      | h :: t ->
          let new_mp = Map.set mp ~data:(i, j) ~key:h in
          process_row new_mp i (j + 1) t
      | [] -> mp
  in

  let rec iter_rows mp i gd =
    match gd with
      | h :: t ->
          let new_mp = process_row mp i 0 h in
          iter_rows new_mp (i + 1) t
      | [] -> mp
  in

  iter_rows mp 0 gd

let cells_to_mvs (bi, bj) (si, sj) (ei, ej) =
  let i = si - ei in
  let j = sj - ej in

  let in_bad_row = si = bi && ej = bj in
  let in_bad_col = sj = bj && ei = bi in

  let repeat s n =
    let rec rp s1 n1 = if n1 = 0 then s1 else rp (s1 ^ s) (n1 - 1) in
    rp "" n
  in

  let i_string =
    let count = Int.abs i in
    if i < 0 then repeat "v" count else if i = 0 then "" else repeat "^" count
  in

  let j_string =
    let count = Int.abs j in
    if j < 0 then repeat ">" count else if j = 0 then "" else repeat "<" count
  in

  let row_first = j_string ^ i_string ^ "A" in
  let col_first = i_string ^ j_string ^ "A" in

  if in_bad_row then col_first
  else if in_bad_col then row_first
  else if j > 0 then row_first
  else col_first

let line_to_moves mp line =
  let chars = String.to_list line in
  let bad_cell = Map.find_exn mp '_' in
  let _, output =
    List.fold_left chars ~init:('A', "") ~f:(fun (last_char, out) curr_char ->
        let s = Map.find_exn mp last_char in
        let e = Map.find_exn mp curr_char in
        let mvs = cells_to_mvs bad_cell s e in
        (curr_char, out ^ mvs))
  in
  output

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let numpad = grid_to_map grid_nums in
  let mvpad = grid_to_map grid_mvs in

  let s = In_channel.read_all file in
  let lines = s |> String.strip |> String.split_lines in

  let do_map a s =
    let cal_str =
      s |> line_to_moves numpad |> line_to_moves mvpad |> line_to_moves mvpad
    in
    let len = cal_str |> String.length in

    a + (len * get_number_prefix s)
  in

  List.fold_left lines ~init:0 ~f:do_map |> printf "%d\n";
  ()
