open Base
open Stdio
open Str
open Re

let default = "input.txt"
let big_ol_num = 10_000_000_000_000

(* --- *)
let _split_on_whitespace s = s |> Str.split (Str.regexp "\n\n")

(* --- tpl --- *)
let add_tpl (i, j) (h, k) = (i + h, j + k)
let eq_tpl (i, j) (h, k) = equal i h && equal j k
let flip_tpl (i, j) = (-i, -j)

(* --- helpers --- *)
let cache = Stdlib.Hashtbl.create 100000000
let bestRes = ref None
let makeKey (i, j) = (i, j)
let get_sign i = if i < 0 then -1 else 1

(* --- print --- *)
let print_cell lbl (i, j) = printf "%s - %d:%d\n" lbl i j
let print_cell_2 (i, j) = printf "%d:%d " i j

let col_results opt_a opt_b =
  if (not (Option.is_none opt_a)) && not (Option.is_none opt_b) then
    Some (Int.min (Option.value_exn opt_a) (Option.value_exn opt_b))
  else if not (Option.is_none opt_a) then Some (Option.value_exn opt_a)
  else if not (Option.is_none opt_b) then Some (Option.value_exn opt_b)
  else None

let shift_back_start ((t1, t2) as target) ((s1, s2) as slide) =
  let shift_amount = t1 - t2 in
  let shift_inc = s1 - s2 in

  let dir = get_sign (shift_inc * shift_amount) in

  let steps = Int.abs shift_amount / Int.abs shift_inc in
  (* printf "steps: %d\n" steps; *)
  let new_target = (t1 - (s1 * dir * steps), t2 - (s2 * dir * steps)) in
  let cost_adj = steps * dir * 2 in
  (* print_cell "new target" new_target; *)
  (* printf "cost_adj: %d\n" cost_adj; *)
  (new_target, cost_adj)

let find_start ((a1, a2) as a) ((b1, b2) as b) ((t1, t2) as target) start =
  let ((c1, c2) as c) = (a1 + b1, a2 + b2) in
  let ((s1, s2) as slide) = (a1 - b1, a2 - b2) in

  let rec find n cost ((t1, t2) as target) =
    (* This is a dirty hack for if we go oob *)
    if t1 < 0 || t2 < 0 then
      let () = print_cell "oob:" start in
      (0, (1, 1))
    else if Int.equal n 0 then (cost, target)
    else
      let delta = Int.min (t1 / c1) (t2 / c2) in
      let ((d1, d2) as d) = (t1 - (delta * c1), t2 - (delta * c2)) in

      (* print_cell "target" target; *)
      let t1 = d1 in
      let t2 = d2 in
      let target = (t1, t2) in
      let cost = cost + (delta * 4) in

      (* print_cell "a" a; *)
      (* print_cell "b" b; *)
      (* print_cell "c" c; *)
      (* print_cell "d" d; *)
      (* print_cell "slide" slide; *)
      (* printf "delta: %d\n" delta; *)
      (* printf "cost: %d\n" cost; *)
      (* print_cell "before shift target" target; *)
      let new_target, cost_adj = shift_back_start target slide in
      let new_cost = cost + cost_adj in

      (* printf "new cost: %d\n" new_cost; *)
      (* printf "new target: %d\n" new_cost; *)
      (* printf "\n"; *)
      find (n - 1) new_cost new_target
  in
  find 15 0 target

let win_prizes ((a1, a2) as a) ((b1, b2) as b) ((t1, t2) as t) =
  Stdlib.Hashtbl.clear cache;
  bestRes := None;

  let cost, ((t1, t2) as target) =
    find_start a b (t1 + big_ol_num, t2 + big_ol_num) t
    (* find_start a b (t1, t2) *)
  in

  (* printf "cost after find: %d\n" cost; *)
  let rec win n ((ci, cj) as curr) = function
    | cost when eq_tpl curr target ->
        (* printf "cost final: %d\n" cost; *)
        bestRes := Some (Int.min cost (Option.value ~default:cost !bestRes));
        Some cost
    | cost when cost > Option.value ~default:cost !bestRes -> None
    | _ when Int.equal n 0 -> None
    | _ when ci > t1 || cj > t2 -> None
    | cost ->
        let o = Stdlib.Hashtbl.find_opt cache (makeKey curr) in
        if (not (Option.is_none o)) && Option.value_exn o <= cost then None
        else
          (* let () = printf "%d:%d - %d\n" ci cj cost in *)
          let () = Stdlib.Hashtbl.add cache (makeKey curr) cost in
          let opt_b = win (n - 1) (add_tpl curr b) (cost + 1) in
          let opt_a = win (n - 1) (add_tpl curr a) (cost + 3) in

          let res = col_results opt_a opt_b in
          res
  in
  (* Some cost *)
  win 200 (0, 0) cost

let parse_line line =
  let re =
    let open Re in
    alt [ rg '0' '9' ] |> rep1 |> group |> compile
  in

  let fin =
    match Re.matches re line with
    | a1 :: a2 :: b1 :: b2 :: t1 :: t2 :: rest ->
        ( (a1 |> Int.of_string, a2 |> Int.of_string),
          (b1 |> Int.of_string, b2 |> Int.of_string),
          (t1 |> Int.of_string, t2 |> Int.of_string) )
    | _ -> failwith "bad input"
  in
  fin

let () =
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines = s |> String.strip |> _split_on_whitespace in

  let sub_lines =
    match lines with
    | t1 :: m1 :: __ :: m2 :: ___ -> [ t1 ]
    | _ -> failwith "bad lines"
  in

  let sub_lines = [ List.nth_exn lines 23 ] in
  let sub_lines = lines in
  let count =
    sub_lines |> List.map ~f:parse_line
    |> List.map ~f:(fun (t, a, b) -> win_prizes t a b)
    |> List.filter ~f:Option.is_some
    |> List.map ~f:Stdlib.Option.get
    |> List.fold_left ~f:( + ) ~init:0
  in

  printf "%d\n" count
