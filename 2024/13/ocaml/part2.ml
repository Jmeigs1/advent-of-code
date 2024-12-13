open Base
open Stdio
open Str
open Re

(* Need to implemnt a* when not tired *)

let default = "input2.txt"
let _split_on_whitespace s = s |> Str.split (Str.regexp "\n\n")
let add_tpl (i, j) (h, k) = (i + h, j + k)
let eq_tpl (i, j) (h, k) = equal i h && equal j k
let makeKey (i, j) = (i, j)
let cache = Stdlib.Hashtbl.create 100000000
let bestRes = ref None

let col_results opt_a opt_b =
  if (not (Option.is_none opt_a)) && not (Option.is_none opt_b) then
    Some (Int.min (Option.value_exn opt_a) (Option.value_exn opt_b))
  else if not (Option.is_none opt_a) then Some (Option.value_exn opt_a)
  else if not (Option.is_none opt_b) then Some (Option.value_exn opt_b)
  else None

let win_prizes a b (t1, t2) =
  Stdlib.Hashtbl.clear cache;
  bestRes := None;

  let t1 = t1 + 10000000000000 in
  let t2 = t2 + 10000000000000 in
  let target = (t1 + 10000000000000, t2 + 10000000000000) in

  let rec win n ((ci, cj) as curr) = function
    | cost when eq_tpl curr target ->
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
  win 100000000000000 (0, 0) 0

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
    | _ -> failwith "asdf"
  in
  fin

let () =
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines = s |> String.strip |> _split_on_whitespace in

  let count =
    [ List.hd_exn lines ]
    |> List.map ~f:parse_line
    |> List.map ~f:(fun (t, a, b) -> win_prizes t a b)
    |> List.filter ~f:Option.is_some
    |> List.map ~f:Stdlib.Option.get
    |> List.fold_left ~f:( + ) ~init:0
  in

  printf "%d\n" count
