open Core

type cell = int * int

(* --- tpl --- *)
let add_tpl (i, j) (h, k) = (i + h, j + k)
let mul_tpl (i, j) (h, k) = (i * h, j * k)
let abs_diff_tpl (i, j) (h, k) = Int.abs (i - h) + Int.abs (j - k)

(* --- dirs --- *)
let dirs = [| (0, 1); (-1, 0); (0, -1); (1, 0) |]
let get_next_dir i = i + (1 % 4)

let dirs_to_moves i cost =
  [
    (1 + cost, i, dirs.(i));
    (1000 + cost, (i - 1) % 4, dirs.((i - 1) % 4));
    (1000 + cost, (i + 1) % 4, dirs.((i + 1) % 4));
    (2000 + cost, (i + 2) % 4, dirs.((i + 2) % 4));
  ]

let map_from_grid lst =
  let start_mp = Map.Poly.empty in
  let start_point = ref (0, 0) in
  let end_point = ref (0, 0) in

  let rec process_line mp line i j =
    if Int.equal j (String.length line) then mp
    else
      match line.[j] with
        | '#' ->
            let new_mp = Map.set mp ~key:(i, j) ~data:'#' in
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

  feed_line start_mp 0 lst

let traverse mp start_dir start_point end_point =
  let rec traverse cost seen dir start =
    if Poly.equal start end_point then (cost, seen)
    else
      let moves = dirs_to_moves dir cost in
      let open_moves =
        moves
        |> List.map ~f:(fun (c, dir, v) -> (c, dir, add_tpl v start))
        |> List.filter ~f:(fun (c, dir, s) ->
               let v = Map.find mp s in
               Option.is_some v && not (Char.equal (Option.value_exn v) '#'))
        |> List.filter ~f:(fun (c, dir, s) -> not (Set.mem seen s))
      in
      (cost, seen)
  in
  traverse 0 Set.Poly.empty start_dir start_point

let () =
  let default = "input2.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines = s |> String.strip |> String.split_lines in

  let start_dir = 0 in
  let start_point, end_point, mp = map_from_grid lines in

  let low_score, _ = traverse mp start_dir start_point end_point in

  printf "score: %d\n" low_score
