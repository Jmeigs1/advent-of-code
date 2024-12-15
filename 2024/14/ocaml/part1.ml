open Core
open Re

(* --- consts --- *)
let gridi = 103
let gridj = 101

(* --- consts --- *)
(* let gridi = 7 *)
(* let gridj = 11 *)

(* --- print --- *)
let printCell lbl (i, j) = Printf.printf "%s - %d:%d\n" lbl i j

(* --- tpl --- *)
let add_tpl (i, j) (h, k) = (i + h, j + k)
let eq_tpl (i, j) (h, k) = equal i h && equal j k
let mul_tpl (i, j) (h, k) = (i * h, j * k)

let build_start line =
  let coord_re =
    let open Re in
    seq [ str "-" |> opt; alt [ rg '0' '9' ] |> rep1 ] |> compile
  in

  let re_matches = Re.matches coord_re line |> List.map ~f:Int.of_string in

  match re_matches with
  | [ p2; p1; v2; v1 ] -> ((p1, p2), (v1, v2))
  | _ -> failwith "bad match build_start"

let project n bot =
  let pos, vel = bot in
  let np_i, np_j = add_tpl pos (mul_tpl vel (n, n)) in
  (* printCell "cell before mod" (np_i, np_j); *)
  let np_i = np_i % gridi in
  let np_j = np_j % gridj in
  (np_i, np_j)

let build_checksum (a1, a2, a3, a4) (pi, pj) =
  let mid_i = gridi / 2 in
  let mid_j = gridj / 2 in

  let c1, c2, c3, c4 =
    if pi > mid_i && pj > mid_j then (1, 0, 0, 0)
    else if pi > mid_i && pj < mid_j then (0, 1, 0, 0)
    else if pi < mid_i && pj > mid_j then (0, 0, 1, 0)
    else if pi < mid_i && pj < mid_j then (0, 0, 0, 1)
    else (0, 0, 0, 0)
  in

  (a1 + c1, a2 + c2, a3 + c3, a4 + c4)

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines = s |> String.strip |> String.split_on_chars ~on:[ '\n' ] in

  lines |> List.map ~f:build_start
  |> List.map ~f:(project 100)
  |> List.fold_left ~f:build_checksum ~init:(0, 0, 0, 0)
  |> fun (a1, a2, a3, a4) -> a1 * a2 * a3 * a4 |> printf "%d\n"
