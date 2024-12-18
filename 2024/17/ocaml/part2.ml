open Core
open Re

let split_to_parts s = s |> Str.split (Str.regexp "\n\n")
let get_nums = Re.(alt [ rg '0' '9' ] |> rep1 |> compile)

let base_8_to_10 s =
  s |> String.to_list
  |> List.map ~f:(fun c -> sprintf "%c" c |> Int.of_string)
  |> List.fold_left ~init:0 ~f:(fun a i -> (a * 8) + i)

(* --- *)
let get_combo combo ra rb rc =
  match combo with
    | 0 -> 0
    | 1 -> 1
    | 2 -> 2
    | 3 -> 3
    | 4 -> ra
    | 5 -> rb
    | 6 -> rc
    | _ -> failwith "bad get_combo"

let do_instruction opcode combo start ra rb rc result =
  match opcode with
    | 0 ->
        let combo_val = get_combo combo ra rb rc in
        let new_ra = ra / Int.pow 2 combo_val in
        (start + 2, new_ra, rb, rc, result)
    | 1 ->
        let new_rb = rb lxor combo in
        (start + 2, ra, new_rb, rc, result)
    | 2 ->
        let combo_val = get_combo combo ra rb rc in
        let new_rb = combo_val % 8 in
        (start + 2, ra, new_rb, rc, result)
    | 3 ->
        (* -- *)
        if ra = 0 then (start + 2, ra, rb, rc, result)
        else (combo, ra, rb, rc, result)
    | 4 ->
        let _ = get_combo combo ra rb rc in
        let new_rb = rb lxor rc in
        (start + 2, ra, new_rb, rc, result)
    | 5 ->
        let combo_val = get_combo combo ra rb rc in
        let print_val = sprintf "%d" (combo_val % 8) in
        (start + 2, ra, rb, rc, result ^ print_val)
    | 6 ->
        let combo_val = get_combo combo ra rb rc in
        let new_rb = ra / Int.pow 2 combo_val in
        (start + 2, ra, new_rb, rc, result)
    | 7 ->
        let combo_val = get_combo combo ra rb rc in
        let new_rc = ra / Int.pow 2 combo_val in
        (start + 2, ra, rb, new_rc, result)
    | _ -> failwith "bad opcode"

let loop_instructions inst ra rb rc =
  let size = Array.length inst in
  let rec loop (i, ra, rb, rc, result) =
    if not (i + 1 < size) then result
    else loop (do_instruction inst.(i) inst.(i + 1) i ra rb rc result)
  in
  loop (0, ra, rb, rc, "")

let find_input inst expected =
  let rb = 0 in
  let rc = 0 in

  let rec do_digit start expected_next n candidates =
    if n = 8 then candidates
    else
      let next_start = sprintf "%s%d" start n in
      let ra = base_8_to_10 next_start in
      let res = loop_instructions inst ra rb rc in
      (* printf "res: %s expected: %c\n" res expected_next; *)
      let n_candidates =
        if Char.equal res.[0] expected_next then next_start :: candidates
        else candidates
      in
      do_digit start expected_next (n + 1) n_candidates
  in

  let rec find i candidates =
    if i < 0 then
      match candidates with
        | h :: tail ->
            List.fold_left tail ~init:(base_8_to_10 h) ~f:(fun a v ->
                Int.min a (base_8_to_10 v))
        | [] -> failwith "found no end. something went wrong"
    else
      let nxt_char = expected.[i] in
      let new_candidats =
        candidates
        |> List.map ~f:(fun c -> (* printf "%s\n" c; *)
                                 do_digit c nxt_char 0 [])
        |> Stdlib.List.flatten
      in
      (* printf "c len %d\n" (List.length new_candidats); *)
      find (i - 1) new_candidats
  in

  find (String.length expected - 1) [ "" ]

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let parts = s |> split_to_parts in
  let _, p2 =
    match parts with
      | [ p1; p2 ] -> (p1, p2)
      | _ -> failwith "bad part split"
  in

  let p2_matches = Re.matches get_nums p2 in

  let instructions =
    Re.matches get_nums p2 |> List.map ~f:Int.of_string |> Array.of_list
  in

  let expected_output = p2_matches |> List.fold_left ~init:"" ~f:( ^ ) in

  let found_ra = find_input instructions expected_output in
  printf "%d\n" found_ra;
  ()
