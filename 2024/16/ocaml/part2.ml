open Core

type cell = int * int

(* --- tpl --- *)
let add_tpl (i, j) (h, k) = (i + h, j + k)
let mul_tpl (i, j) (h, k) = (i * h, j * k)
let abs_diff_tpl (i, j) (h, k) = Int.abs (i - h) + Int.abs (j - k)
let print_cell lbl (i, j) = Printf.printf "%s - %d:%d\n%!" lbl i j

(* --- dirs --- *)
let dirs = [| (0, 1); (-1, 0); (0, -1); (1, 0) |]
let conv_dir d = (d + 2) mod 4

let dirs_to_moves start i cost =
  [
    (1 + cost, i, add_tpl start dirs.(i));
    (1000 + cost, (i - 1) % 4, start);
    (1000 + cost, (i + 1) % 4, start);
    (2000 + cost, (i + 2) % 4, start);
  ]

let map_from_grid lst =
  let start_set = Set.Poly.empty in
  let start_point = ref (0, 0) in
  let end_point = ref (0, 0) in

  let rec process_line mp line i j =
    if Int.equal j (String.length line) then mp
    else
      match line.[j] with
        | '#' ->
            let new_mp = Set.add mp (i, j) in
            process_line new_mp line i (j + 1)
        | 'S' ->
            start_point := (i, j);
            process_line mp line i (j + 1)
        | 'E' ->
            end_point := (i, j);
            process_line mp line i (j + 1)
        | _ -> process_line mp line i (j + 1)
  in

  let rec feed_line mp i lst =
    match lst with
      | [] -> (!start_point, !end_point, mp)
      | h :: t ->
          let new_mp = process_line mp h i 0 in
          feed_line new_mp (i + 1) t
  in

  feed_line start_set 0 lst

let find_min opts =
  let min =
    Map.fold ~init:None
      ~f:(fun ~key ~data a ->
        if Option.is_none a then Some (key, data)
        else
          let ((a_loc, a_dir) as a_key), ((a_cost, _) as a_data) =
            Option.value_exn a
          in
          let cost, _ = data in
          if a_cost > cost then Some (key, data) else Some (a_key, a_data))
      opts
  in
  match min with
    | None -> failwith "bad find_min"
    | Some v -> v

let traverse (start : cell) start_cost finish blocks =
  let seen = Set.Poly.empty in
  let options =
    Map.Poly.empty |> Map.set ~key:(start, 0) ~data:(start_cost, [])
  in

  let rec traverse seen options =
    match Map.is_empty options with
      | true -> failwith "ran out of options in traverse"
      | false ->
          let (current_cell, current_dir), (current_cost, current_parents) =
            find_min options
          in
          if Poly.equal current_cell finish then
            ( current_cost,
              current_cell :: current_parents,
              (current_cell, current_dir) )
          else
            let n_options = Map.remove options (current_cell, current_dir) in
            let n_seen = Set.add seen (current_cell, current_dir) in

            let adj =
              dirs_to_moves current_cell current_dir current_cost
              |> List.filter ~f:(fun (cost, dir, loc) ->
                     not (Set.mem blocks loc || Set.mem n_seen (loc, dir)))
            in

            let n_options =
              List.fold_left adj ~init:n_options ~f:(fun a (cost, dir, cell) ->
                  if not (Map.mem a (cell, dir)) then
                    Map.set a ~key:(cell, dir)
                      ~data:(cost, current_cell :: current_parents)
                  else
                    let old_cost, _ = Map.find_exn a (cell, dir) in
                    if cost = old_cost then
                      let rng = Random.int 2 in
                      match rng with
                        | 1 ->
                            Map.set a ~key:(cell, dir)
                              ~data:(cost, current_cell :: current_parents)
                        | 0 -> a
                        | _ -> failwith "bad rng"
                    else
                      match cost < old_cost with
                        | true ->
                            Map.set a ~key:(cell, dir)
                              ~data:(cost, current_cell :: current_parents)
                        | false -> a)
            in

            traverse n_seen n_options
  in
  traverse seen options

let run_it start_point start_dir end_point blocks limit =
  let rec repeat path i =
    if i >= limit then path
    else
      let low_score, parents, (end_point, end_dir) =
        traverse start_point start_dir end_point blocks
      in
      let path = Set.union path (Set.Poly.of_list parents) in
      repeat path (i + 1)
  in
  repeat Set.Poly.empty 0

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines = s |> String.strip |> String.split_lines in

  let start_dir = 0 in
  let start_point, end_point, blocks = map_from_grid lines in

  let path = run_it start_point start_dir end_point blocks 40 in

  printf "path %d\n" (Set.length path);
  ()
