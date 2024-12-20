open Core
open Gridlib

type cell = int * int

let print_cell lbl (i, j) = Printf.printf "%s - %d:%d\n%!" lbl i j

let rec not_before_in_lst lst v i limit =
  if i > limit then true
  else
    match lst with
      | [] -> failwith "ran out of items in not_before_in_lst"
      | h :: t ->
          if Poly.equal h v then false else not_before_in_lst t v (i + 1) limit

let find_cheats blocks path limit =
  let path_set = Set.Poly.of_list path in

  let rec find path path_set out_lst =
    match path with
      | [] -> out_lst
      | current :: t ->
          let new_path_set = Set.remove path_set current in
          let block_recon_to_path b =
            get_adj b
            |> List.filter ~f:(fun cell -> Set.mem new_path_set cell)
            |> List.filter ~f:(fun cell -> not_before_in_lst t cell 1 limit)
          in
          let candidates =
            get_adj current
            |> List.filter ~f:(Set.mem blocks)
            |> List.map ~f:block_recon_to_path
            |> Stdlib.List.flatten
            |> List.map ~f:(fun f -> (current, f))
          in
          find t new_path_set (candidates @ out_lst)
  in

  Set.Poly.of_list (find path path_set [])

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines = String.split_lines s in

  let size_i = List.length lines in
  let size_j = String.length (List.hd_exn lines) in

  let start, finish, blocks = grid_to_set lines in

  let cost, path = traverse start 0 finish size_i size_j blocks in

  let path = List.rev path in

  let cheats = find_cheats blocks path 100 in

  Set.length cheats |> printf "%d\n%!";

  ()
