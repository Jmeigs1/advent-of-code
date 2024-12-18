open Core
open Re

let split_to_parts s = s |> Str.split (Str.regexp "\n\n")
let get_nums = Re.(alt [ rg '0' '9' ] |> rep1 |> compile)

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
        (start + 2, ra, rb, rc, print_val :: result)
    | 6 ->
        let combo_val = get_combo combo ra rb rc in
        let new_rb = ra / Int.pow 2 combo_val in
        (start + 2, ra, new_rb, rc, result)
    | 7 ->
        let combo_val = get_combo combo ra rb rc in
        let new_rc = ra / Int.pow 2 combo_val in
        (start + 2, ra, rb, new_rc, result)
    | _ -> failwith "bad opcode"

let rec join separator = function
  | [] -> ""
  | [ str ] -> str
  | "" :: strs -> join separator strs
  | str :: strs -> str ^ separator ^ join separator strs

let loop_instructions inst ra rb rc expected =
  let size = Array.length inst in
  let rec loop (i, ra, rb, rc, result) =
    let test_str = List.rev result |> join "," in

    if String.equal test_str expected then Some test_str
    else if not (i + 1 < size) then
      let () = printf "%s\n%!" expected in
      let () = printf "%s\n%!" test_str in
      None
    else loop (do_instruction inst.(i) inst.(i + 1) i ra rb rc result)
  in
  loop (0, ra, rb, rc, [])

let rec find_input ra inst rb rc expected =
  if ra % 10000 = 0 then printf "count: %d\n%!" ra else ();
  let res = loop_instructions inst ra rb rc expected in
  if Option.is_some res then (ra, Option.value_exn res)
  else
    (* -- *)
    (* find_input (ra + 1) inst rb rc expected *)
    failwith "asdf"

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let parts = s |> split_to_parts in
  let p1, p2 =
    match parts with
      | [ p1; p2 ] -> (p1, p2)
      | _ -> failwith "bad part split"
  in

  let registers = Re.matches get_nums p1 |> List.map ~f:Int.of_string in
  let ra, rb, rc =
    match registers with
      | [ ra; rb; rc ] -> (ra, rb, rc)
      | _ -> failwith "bad register split"
  in

  let p2_matches = Re.matches get_nums p2 in

  let instructions =
    Re.matches get_nums p2 |> List.map ~f:Int.of_string |> Array.of_list
  in

  let expected_output = p2_matches |> join "," in

  (* --- I know the answer is between 2**45 and 2**48 not sure how to do
     better--- *)
  let found_ra, _ =
    find_input (Int.pow 2 45) instructions rb rc expected_output
  in
  printf "%d\n" found_ra;
  ()
