open Core
open Re

let create_or_append_set k v a =
  match Map.mem a k with
    | true ->
        let old_val = Map.find_exn a k in
        Map.set a ~key:k ~data:(Set.add old_val v)
    | false -> Map.set a ~key:k ~data:(Set.Poly.of_list [ v ])

let build_mp edges =
  let aggr_fn a (v1, v2) =
    a |> create_or_append_set v1 v2 |> create_or_append_set v2 v1
  in

  List.fold_left edges ~init:Map.Poly.empty ~f:aggr_fn

let combinations lst =
  let rec aux acc = function
    | []
    | [ _ ] ->
        acc
    | x :: xs ->
        let acc' =
          List.fold_left ~f:(fun acc y -> (x, y) :: acc) ~init:acc xs
        in
        aux acc' xs
  in
  aux [] lst

let build_choose_3 mp start_key =
  let children = Map.find_exn mp start_key |> Set.to_list in
  let pairs = combinations children in

  let valid_pairs =
    pairs |> List.filter ~f:(fun (a, b) -> Set.mem (Map.find_exn mp a) b)
  in

  valid_pairs
  |> List.fold_left ~init:Set.Poly.empty ~f:(fun acc (a, b) ->
         Set.add acc (List.sort ~compare:String.compare [ start_key; a; b ]))

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let edges =
    s |> String.strip |> String.split_lines
    |> List.map ~f:(String.split ~on:'-')
    |> List.map ~f:(function
         | [ a; b ] -> (a, b)
         | _ -> failwith "bad tpl")
  in

  let mp = build_mp edges in

  let start_keys =
    Map.key_set mp |> Set.filter ~f:(fun s -> Char.equal s.[0] 't')
  in

  let combos =
    start_keys
    |> Set.fold ~init:Set.Poly.empty ~f:(fun acc k ->
           Set.union acc (build_choose_3 mp k))
  in

  printf "%d\n" (Set.length combos);
  ()
