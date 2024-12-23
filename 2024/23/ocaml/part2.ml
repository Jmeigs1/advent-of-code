open Core
open Re

let join lst sep =
  match lst with
    | h :: t -> List.fold_left ~init:h ~f:(fun a s -> a ^ sep ^ s) t
    | [] -> ""

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

let rec combinations n lst =
  if n = 0 then [ [] ]
  else
    match lst with
      | [] -> []
      | x :: xs ->
          let comb_with_x =
            List.map ~f:(fun c -> x :: c) (combinations (n - 1) xs)
          in
          let comb_without_x = combinations n xs in
          comb_with_x @ comb_without_x

let build_choose_3 mp start_key =
  let children = Map.find_exn mp start_key |> Set.to_list in
  let pairs = combinations 2 children in

  let valid_pairs =
    pairs
    |> List.map ~f:(function
         | [ a; b ] -> (a, b)
         | _ -> failwith "asdf")
    |> List.filter ~f:(fun (a, b) -> Set.mem (Map.find_exn mp a) b)
  in

  valid_pairs
  |> List.fold_left ~init:Set.Poly.empty ~f:(fun acc (a, b) ->
         Set.add acc (List.sort ~compare:String.compare [ start_key; a; b ]))

let add_next_keys mp keys combo =
  let not_in_map cv k = Set.mem (Map.find_exn mp cv) k |> not in
  let cand_keys =
    Set.filter keys ~f:(fun k ->
        List.find combo ~f:(fun c -> not_in_map c k) |> function
        | Some _ -> false
        | None -> true)
  in

  Set.fold cand_keys ~init:Set.Poly.empty ~f:(fun acc key ->
      Set.add acc (List.sort ~compare:String.compare (key :: combo)))

let rec find_biggest mp keys combos i =
  printf "testing grid size: %d\n%!" i;
  match Set.length combos with
    | 1 -> Set.choose_exn combos
    | 0 -> failwith "ran out of combos in find_biggest"
    | _ ->
        let n_combos =
          Set.fold ~init:Set.Poly.empty
            ~f:(fun acc v -> Set.union acc (add_next_keys mp keys v))
            combos
        in
        find_biggest mp keys n_combos (i + 1)

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

  let start_keys = Map.key_set mp in

  let combos_of_3 =
    start_keys
    |> Set.fold ~init:Set.Poly.empty ~f:(fun acc k ->
           Set.union acc (build_choose_3 mp k))
  in

  let biggest = find_biggest mp start_keys combos_of_3 3 in

  printf "%s\n" (join biggest ",");
  ()
