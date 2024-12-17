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

let do_instruction opcode combo start ra rb rc =
  match opcode with
    | 0 ->
        let combo_val = get_combo combo ra rb rc in
        let new_ra = ra / Int.pow 2 combo_val in
        (start + 2, new_ra, rb, rc)
    | 1 ->
        let new_rb = rb lxor combo in
        (start + 2, ra, new_rb, rc)
    | 2 ->
        let combo_val = get_combo combo ra rb rc in
        let new_rb = combo_val % 8 in
        (start + 2, ra, new_rb, rc)
    | 3 ->
        (* -- *)
        if ra = 0 then (start + 2, ra, rb, rc) else (combo, ra, rb, rc)
    | 4 ->
        let _ = get_combo combo ra rb rc in
        let new_rb = rb lxor rc in
        (start + 2, ra, new_rb, rc)
    | 5 ->
        let combo_val = get_combo combo ra rb rc in
        let () = printf "%d," (combo_val % 8) in
        (start + 2, ra, rb, rc)
    | 6 ->
        let combo_val = get_combo combo ra rb rc in
        let new_rb = ra / Int.pow 2 combo_val in
        (start + 2, ra, new_rb, rc)
    | 7 ->
        let combo_val = get_combo combo ra rb rc in
        let new_rc = ra / Int.pow 2 combo_val in
        (start + 2, ra, rb, new_rc)
    | _ -> failwith "bad opcode"

let loop_instructions inst ra rb rc =
  let size = Array.length inst in
  let rec loop (i, ra, rb, rc) =
    if not (i + 1 < size) then ()
    else loop (do_instruction inst.(i) inst.(i + 1) i ra rb rc)
  in
  loop (0, ra, rb, rc)

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

  let instructions =
    Re.matches get_nums p2 |> List.map ~f:Int.of_string |> Array.of_list
  in

  (* Array.iter instructions ~f:(printf "%d\n"); *)
  loop_instructions instructions ra rb rc;
  printf "\n";
  ()
