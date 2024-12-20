open Core

(* --- tpl --- *)
let abs_diff_tpl (i, j) (h, k) = Int.abs (i - h) + Int.abs (j - k)
let add_tpl (i, j) (h, k) = (i + h, j + k)
let eq_tpl (i, j) (h, k) = i = h && j = k
let mul_tpl (i, j) (h, k) = (i * h, j * k)

let is_in_bounds size_i size_j cell =
  let ci, cj = cell in
  let i = ci >= 0 && ci < size_i in
  let j = cj >= 0 && cj < size_j in
  i && j

let get_adj start =
  let dirs = [ (1, 0); (0, 1); (-1, 0); (0, -1) ] in
  dirs |> List.map ~f:(add_tpl start)

let find_min opts =
  let min =
    Map.fold ~init:None
      ~f:(fun ~key ~data a ->
        if Option.is_none a then Some (key, data)
        else
          let a_key, ((a_cost, _) as a_data) = Option.value_exn a in
          let cost, _ = data in
          if a_cost > cost then Some (key, data) else Some (a_key, a_data))
      opts
  in
  match min with
    | None -> failwith "bad find_min"
    | Some (cell, cost) -> (cell, cost)

let traverse start start_cost finish size_i size_j blocks =
  let seen = Set.Poly.empty in
  let options = Map.Poly.empty |> Map.set ~key:start ~data:(start_cost, []) in

  let rec traverse seen options =
    match Map.is_empty options with
      | true -> failwith "ran out of options in traverse"
      | false ->
          let current_cell, (current_cost, current_parents) =
            find_min options
          in
          if Poly.equal current_cell finish then
            (current_cost, current_cell :: current_parents)
          else
            let n_options = Map.remove options current_cell in
            let n_seen = Set.add seen current_cell in

            let adj =
              get_adj current_cell
              |> List.filter ~f:(fun c -> not (Set.mem blocks c))
              |> List.filter ~f:(is_in_bounds size_i size_j)
              |> List.filter ~f:(fun c -> not (Set.mem n_seen c))
              |> List.map ~f:(fun x ->
                     (x, (current_cost + 1, current_cell :: current_parents)))
            in

            let n_options =
              List.fold_left adj ~init:n_options
                ~f:(fun a (cell, ((cost, parents) as data)) ->
                  if not (Map.mem a cell) then Map.set a ~key:cell ~data
                  else
                    let old_cost, _ = Map.find_exn a cell in
                    match cost < old_cost with
                      | true -> Map.set a ~key:cell ~data
                      | false -> a)
            in

            traverse n_seen n_options
  in
  traverse seen options

let grid_to_set lst =
  let start_set = Set.Poly.empty in
  let start_point = ref (0, 0) in
  let end_point = ref (0, 0) in

  let rec process_line set line i j =
    if Int.equal j (String.length line) then set
    else
      match line.[j] with
        | '#' ->
            let new_set = Set.add set (i, j) in
            process_line new_set line i (j + 1)
        | 'S' ->
            start_point := (i, j);
            process_line set line i (j + 1)
        | 'E' ->
            end_point := (i, j);
            process_line set line i (j + 1)
        | _ -> process_line set line i (j + 1)
  in

  let rec feed_line set i lst =
    match lst with
      | [] -> (!start_point, !end_point, set)
      | h :: t ->
          let new_set = process_line set h i 0 in
          feed_line new_set (i + 1) t
  in

  feed_line start_set 0 lst
