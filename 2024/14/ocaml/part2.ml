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

let replace_char str index new_char =
  let len = String.length str in
  if index < 0 || index >= len then failwith "Index out of bounds"
  else
    let before = Stdlib.String.sub str 0 index in
    let after = Stdlib.String.sub str (index + 1) (len - index - 1) in
    before ^ String.make 1 new_char ^ after

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

let print_grid i lst =
  let arr =
    Array.init gridi ~f:(fun _ -> String.init gridj ~f:(fun _ -> '-'))
  in

  let rec add_points lst =
    match lst with
    | h :: tail ->
        let pi, pj = h in
        let start = arr.(pi) in
        let rep = replace_char start pj '+' in
        arr.(pi) <- rep;
        add_points tail
    | [] -> ()
    | _ -> failwith "bad add_pints"
  in

  let rec printLoop n =
    if Int.equal n (Array.length arr) then ()
    else
      let () = printf "%s\n" arr.(n) in
      printLoop (n + 1)
  in

  let () = printf "Grid at iter: %d\n" i in
  add_points lst;
  printLoop 0

let rec loop start fin lst =
  if Int.equal start fin then ()
  else
    let () = lst |> List.map ~f:(project start) |> print_grid start in
    let () = loop (start + 1) fin lst in
    ()

(* This will generate ~51 Mb text files when piped - Then used grep *)
let step = 5

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines = s |> String.strip |> String.split_on_chars ~on:[ '\n' ] in

  let lst = lines |> List.map ~f:build_start in

  lst |> loop (5000 * step) (5000 * (step + 1))
