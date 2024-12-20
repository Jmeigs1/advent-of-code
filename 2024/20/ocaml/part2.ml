open Core
open Gridlib

type cell = int * int

let print_cell lbl (i, j) = Printf.printf "%s - %d:%d\n%!" lbl i j

let get_cells_in_range path_set size (center_i, center_j) =
  let rec go i j out_lst =
    if i > center_i + size then out_lst
    else if j > center_j + size then go (i + 1) (center_j - size) out_lst
    else
      let l_i = i - center_i in
      let l_j = j - center_j in

      let cost = Int.abs l_i + Int.abs l_j in
      if cost > size then go i (j + 1) out_lst
      else
        let new_out_lst =
          if Set.mem path_set (i, j) then (cost, (i, j)) :: out_lst else out_lst
        in
        go i (j + 1) new_out_lst
  in
  go (center_i - size) (center_j - size) []

let rec find_in_lst lst v i =
  match lst with
    | [] -> failwith "ran out of items in find_in_lst"
    | h :: t -> if Poly.equal h v then i else find_in_lst t v (i + 1)

let find_cheats blocks path limit size_i size_j =
  let path_set = Set.Poly.of_list path in

  let rec find path path_set out_lst =
    match path with
      | [] -> out_lst
      | current :: t ->
          let new_path_set = Set.remove path_set current in

          let candidates =
            current
            |> get_cells_in_range new_path_set 20
            |> List.filter ~f:(fun (cost, cell) ->
                   find_in_lst t cell 1 - cost >= limit)
            |> List.map ~f:(fun (_, cell) -> (current, cell))
            |> Set.Poly.of_list
          in

          find t new_path_set (Set.union candidates out_lst)
  in

  find path path_set Set.Poly.empty

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

  let cheats = find_cheats blocks path 100 size_i size_j in

  Set.length cheats |> printf "%d\n%!";

  ()
