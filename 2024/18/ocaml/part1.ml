open Core
open Re

type cell = int * int

let split_to_parts s = s |> Str.split (Str.regexp "\n\n")
let get_nums = Re.matches Re.(alt [ rg '0' '9' ] |> rep1 |> compile)

(* --- tpl --- *)
let abs_diff_tpl (i, j) (h, k) = Int.abs (i - h) + Int.abs (j - k)
let add_tpl (i, j) (h, k) = (i + h, j + k)
let eq_tpl (i, j) (h, k) = i = h && j = k
let mul_tpl (i, j) (h, k) = (i * h, j * k)

let to_pairs lst =
  let rec to_pairs lst out =
    match lst with
      | h :: h2 :: tail -> to_pairs tail ((h, h2) :: out)
      | _ -> out
  in
  to_pairs lst []

let size_x = 71
let size_y = 71
let start = (0, 0)
let finish = (70, 70)
let cap = 1024

(* --- alt --- *)
(* let size_x = 7 *)
(* let size_y = 7 *)
(* let start = (0, 0) *)
(* let finish = (6, 6) *)
(* let cap = 12 *)

let is_in_bounds cell =
  let cx, cy = cell in
  let x = cx >= 0 && cx < size_x in
  let y = cy >= 0 && cy < size_y in
  x && y

let get_adj start =
  let dirs = [ (1, 0); (0, 1); (-1, 0); (0, -1) ] in
  dirs |> List.map ~f:(add_tpl start)

let find_min opts =
  let min =
    Map.fold ~init:None
      ~f:(fun ~key ~data a ->
        if Option.is_none a then Some (key, data)
        else
          let a_key, a_data = Option.value_exn a in
          if a_data > data then Some (key, data) else Some (a_key, a_data))
      opts
  in
  match min with
    | None -> failwith "bad find_min"
    | Some (cell, cost) -> (cell, cost)

let traverse blocks =
  let seen = Set.Poly.empty in
  let options = Map.Poly.empty |> Map.set ~key:start ~data:0 in

  let rec traverse seen options =
    match Map.is_empty options with
      | true -> failwith "ran out of options in traverse"
      | false ->
          let current_cell, current_cost = find_min options in
          if Poly.equal current_cell finish then current_cost
          else
            let n_options = Map.remove options current_cell in
            let n_seen = Set.add seen current_cell in

            (* printf "Options:\n"; *)
            (* Map.iter_keys options ~f:(fun (i, j) -> printf "%d:%d\n" i j); *)
            (* let adj_test = *)
            (*   get_adj current_cell *)
            (*   |> List.filter ~f:(fun c -> not (Set.mem blocks c)) *)
            (* in *)
            let adj =
              get_adj current_cell
              |> List.filter ~f:(fun c -> not (Set.mem blocks c))
              |> List.filter ~f:is_in_bounds
              |> List.filter ~f:(fun c -> not (Set.mem n_seen c))
              |> List.map ~f:(fun x -> (x, current_cost + 1))
            in

            (* printf "Adj :\n"; *)
            (* List.iter adj_test ~f:(fun (i, j) -> printf "%d:%d\n" i j); *)
            let n_options =
              List.fold_left adj ~init:n_options ~f:(fun a (cell, cost) ->
                  if not (Map.mem a cell) then Map.set a ~key:cell ~data:cost
                  else
                    let old_cost = Map.find_exn a cell in
                    match cost < old_cost with
                      | true -> Map.set a ~key:cell ~data:cost
                      | false -> a)
            in

            (* printf "New options:\n"; *)
            (* Map.iter_keys n_options ~f:(fun (i, j) -> printf "%d:%d\n" i j); *)
            (* printf "\n"; *)
            traverse n_seen n_options
  in
  traverse seen options

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in

  let blocks =
    get_nums s |> List.map ~f:Int.of_string |> to_pairs |> List.rev
    |> List.filteri ~f:(fun i _ -> i < cap)
    |> Set.Poly.of_list
  in

  (* Set.iter blocks ~f:(fun (i, j) -> printf "%d:%d\n" i j); *)
  let cost = traverse blocks in
  printf "%d\n" cost;
  ()
