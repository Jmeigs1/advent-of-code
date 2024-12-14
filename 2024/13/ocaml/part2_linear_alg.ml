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

let win_prizes ((a1, a2) as a) ((b1, b2) as b) ((t1, t2) as t) =
  let t1 = big_ol_num + t1 in
  let t2 = big_ol_num + t2 in

  let n = (t1 * b2) - (t2 * b1) in
  let det = (a1 * b2) - (a2 * b1) in

  if Int.equal det 0 then None
  else
    let a = n / det in
    let rem = Int.abs n % Int.abs det in
    let b = (t1 - (a1 * a)) / b1 in
    let cost = (a * 3) + b in

    printf "a: %d\n" a;
    printf "b: %d\n" b;
    printf "rem: %d\n" rem;
    printf "cost: %d\n" cost;
    printf "\n";

    if Int.equal rem 0 then Some cost else None

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

  let count =
    lines |> List.map ~f:parse_line
    |> List.map ~f:(fun (t, a, b) -> win_prizes t a b)
    |> List.filter ~f:Option.is_some
    |> List.map ~f:Stdlib.Option.get
    |> List.fold_left ~f:( + ) ~init:0
  in

  printf "%d\n" count
