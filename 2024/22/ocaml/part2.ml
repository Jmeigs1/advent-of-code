open Core
open Re

let split_to_parts s = s |> Str.split (Str.regexp "\n\n")
let get_nums = Re.(alt [ rg '0' '9' ] |> rep1 |> compile)

let get_next sn =
  let mix sn n = n lxor sn in
  let prune n = n mod 16777216 in

  let p1 = sn * 64 |> mix sn |> prune in
  let p2 = p1 / 32 |> mix p1 |> prune in
  let p3 = p2 * 2048 |> mix p2 |> prune in
  p3

let make_list_of_n fn limit (start : int) =
  let rec get_nth i v out =
    if i >= limit then out
    else
      let n_val = fn v in
      get_nth (i + 1) n_val ((n_val % 10) :: out)
  in
  get_nth 0 start []

let lst_to_4_change_map lst =
  let mp = Map.Poly.empty in
  let rec loop lst mp =
    match lst with
      | h1 :: (h2 :: h3 :: h4 :: h5 :: rest as tail) ->
          let key =
            ((h1 - h2 + 10) * 20 * 20 * 20)
            + ((h2 - h3 + 10) * 20 * 20)
            + ((h3 - h4 + 10) * 20)
            + h4 - h5 + 10
          in
          let new_mp = Map.set mp ~key ~data:h5 in
          loop tail new_mp
      | _ -> mp
  in
  loop lst mp

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines =
    s |> String.strip |> String.split_lines |> List.map ~f:Int.of_string
  in

  let lists =
    lines
    |> List.map ~f:(make_list_of_n get_next 2000)
    |> List.map ~f:lst_to_4_change_map
  in

  let mp =
    List.fold_left lists ~init:Map.Poly.empty ~f:(fun a_mp v_mp ->
        Map.merge a_mp v_mp ~f:(fun ~key data ->
            match data with
              | `Left value -> Some value
              | `Right value -> Some value
              | `Both (v1, v2) -> Some (v1 + v2)))
  in

  let key, max_value =
    Map.fold mp ~init:(0, Int.min_value) ~f:(fun ~key ~data (k, v) ->
        if data > v then (key, data) else (k, v))
  in

  (* let k1, k2, k3, k4 = key in *)
  (* printf "key %d %d %d %d\n" k1 k2 k3 k4; *)
  printf "%d\n" max_value;
  ()
