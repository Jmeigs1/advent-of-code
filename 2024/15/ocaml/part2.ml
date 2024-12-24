open Core
open Re

(* change this to animate *)
let visualize = true

(* -- -- *)
let split_parts s = s |> Str.split (Str.regexp "\n\n")
let to_char_list s = List.init (String.length s) ~f:(String.get s)

(* --- tpl --- *)
let add_tpl (i, j) (h, k) = (i + h, j + k)
let eq_tpl (i, j) (h, k) = i = h && j = k
let mul_tpl (i, j) (h, k) = (i * h, j * k)
let key_tpl (i, j) = sprintf "%d:%d" i j

let from_key_tpl k =
  let vals = k |> String.split ~on:':' |> List.map ~f:Int.of_string in
  match vals with
    | [ a; b ] -> (a, b)
    | _ -> failwith "bad from_key_tpl"

let get_mp_tpl mp t = Map.find mp t

(* --- print --- *)
let print_cell lbl (i, j) = Printf.printf "%s - %d:%d\n" lbl i j

let replace_char str index new_char =
  let len = String.length str in
  if index < 0 || index >= len then failwith "Index out of bounds"
  else
    let before = Stdlib.String.sub str 0 index in
    let after = Stdlib.String.sub str (index + 1) (len - index - 1) in
    before ^ String.make 1 new_char ^ after

let rec first_last = function
  | [] -> failwith "empty list - first_last"
  | [ e ] -> (e, e)
  | [ e1; e2 ] -> (e1, e2)
  | e1 :: _ :: r -> first_last (e1 :: r)

let print_grid boundi boundj robot lst =
  let boundj = boundj * 2 in
  let arr =
    Array.init boundi ~f:(fun _ -> String.init boundj ~f:(fun _ -> '.'))
  in

  let rec add_points lst =
    match lst with
      | (cell_str, value) :: tail ->
          let cell = from_key_tpl cell_str in
          let pi, pj = cell in
          let start = arr.(pi) in
          let rep = replace_char start pj value in
          arr.(pi) <- rep;
          add_points tail
      | [] -> ()
  in

  let clear_screen () =
    print_string "\027[2J";
    print_string "\027[H";
    ()
  in

  let ri, rj = robot in

  arr.(ri) <- replace_char arr.(ri) rj '@';

  let rec print_loop n =
    if Int.equal n (Array.length arr) then ()
    else
      let () = printf "%s\n%!" arr.(n) in
      print_loop (n + 1)
  in
  add_points lst;
  print_loop 0;
  clear_screen ();
  ()

let map_from_grid lst =
  let start_mp = Map.empty (module String) in
  let start_robot = ref (0, 0) in

  let rec process_line mp line i j =
    if Int.equal j (String.length line) then mp
    else
      match line.[j] with
        | '#' ->
            let new_mp =
              Map.set mp ~key:(key_tpl (i, j * 2)) ~data:'#'
              |> Map.set ~key:(key_tpl (i, (j * 2) + 1)) ~data:'#'
            in
            process_line new_mp line i (j + 1)
        | 'O' ->
            let new_mp =
              Map.set mp ~key:(key_tpl (i, j * 2)) ~data:'['
              |> Map.set ~key:(key_tpl (i, (j * 2) + 1)) ~data:']'
            in
            process_line new_mp line i (j + 1)
        | '@' ->
            start_robot := (i, j * 2);
            process_line mp line i (j + 1)
        | _ -> process_line mp line i (j + 1)
  in

  let rec feed_line mp i lst =
    match lst with
      | [] -> (mp, !start_robot)
      | h :: t ->
          let new_mp = process_line mp h i 0 in
          feed_line new_mp (i + 1) t
  in

  feed_line start_mp 0 lst

let mv_to_dir = function
  | 'v' -> (1, 0)
  | '<' -> (0, -1)
  | '^' -> (-1, 0)
  | '>' -> (0, 1)
  | _ -> failwith "bad dir"

let rec remove_duplicates l =
  let rec contains l ((p1, _) as n) =
    match l with
      | [] -> false
      | (p2, _) :: t -> eq_tpl p1 p2 || contains t n
  in
  match l with
    | [] -> []
    | h :: t ->
        let acc = remove_duplicates t in
        if contains acc h then acc else h :: acc

let add_pairs lst =
  let rec add_pairs in_lst out_lst =
    match in_lst with
      | [] -> out_lst
      | (((i, j), v) as orig) :: tail ->
      match v with
        | '[' ->
            let pair = ((i, j + 1), ']') in
            let new_out_lst = pair :: orig :: out_lst in
            add_pairs tail new_out_lst
        | ']' ->
            let pair = ((i, j - 1), '[') in
            let new_out_lst = pair :: orig :: out_lst in
            add_pairs tail new_out_lst
        | _ -> failwith "bad value in add_pairs"
  in
  add_pairs lst []

let get_shift_lst mp start mv =
  let dir = mv_to_dir mv in

  let rec find_i lst start =
    let next = add_tpl start dir in
    let next_val = get_mp_tpl mp (key_tpl next) in

    match next_val with
      | None -> Some lst
      | Some v ->
      match v with
        | '#' -> None
        | v ->
            let new_list = (next, v) :: lst in
            find_i new_list (add_tpl start dir)
  in

  let rec find_j lst starts =
    let next_val = get_mp_tpl mp in

    let any_blocks =
      starts
      |> List.map ~f:(add_tpl dir)
      |> List.map ~f:key_tpl |> List.map ~f:next_val
      |> List.filter ~f:Option.is_some
      |> List.map ~f:Stdlib.Option.get
      |> List.filter ~f:(Char.equal '#')
      |> List.length |> Int.is_positive
    in

    let all_clear =
      starts
      |> List.map ~f:(add_tpl dir)
      |> List.map ~f:key_tpl |> List.map ~f:next_val
      |> List.filter ~f:Option.is_some
      |> List.map ~f:Stdlib.Option.get
      |> List.length |> Int.is_positive |> not
    in

    if any_blocks then None
    else if all_clear then Some lst
    else
      let next_row =
        starts
        |> List.map ~f:(add_tpl dir)
        |> List.map ~f:(fun v -> (v, key_tpl v))
        |> List.map ~f:(fun (v, c) -> (v, next_val c))
        |> List.filter ~f:(fun (_, c) -> Option.is_some c)
        |> List.map ~f:(fun (v, c) -> (v, Stdlib.Option.get c))
        |> List.filter ~f:(fun (_, c) -> Char.equal '[' c || Char.equal ']' c)
        |> add_pairs |> remove_duplicates
      in
      let next_starts = next_row |> List.map ~f:(fun (p, _) -> p) in
      find_j (next_row @ lst) next_starts
  in

  match Char.equal mv '<' || Char.equal mv '>' with
    | true -> find_i [] start
    | false -> find_j [] [ start ]

let rec proces_shift_list mp dir lst =
  match lst with
    | [] -> mp
    | (cell, cell_value) :: t ->
        let new_mp =
          let mp1 = Map.remove mp (key_tpl cell) in
          Map.set mp1 ~key:(key_tpl (add_tpl cell dir)) ~data:cell_value
        in
        proces_shift_list new_mp dir t

let do_move mp start mv =
  let dir = mv_to_dir mv in
  let next_start = add_tpl start dir in
  let next_cell = get_mp_tpl mp (key_tpl next_start) in

  match next_cell with
    | None -> (mp, next_start)
    | Some v when Char.equal v '#' -> (mp, start)
    | Some v when Char.equal v '[' || Char.equal v ']' -> (
        let shift_list = get_shift_lst mp start mv in
        match shift_list with
          | None -> (mp, start)
          | Some v ->
              let new_mp = proces_shift_list mp dir v in
              (new_mp, next_start))
    | Some _ -> failwith "bad next_cell in do_move"

let rec firstk k xs =
  match xs with
    | [ x ] -> [ x ]
    | x :: xs -> if k = 1 then [ x ] else x :: firstk (k - 1) xs
    | [] -> failwith "firstk"

let rec feed_moves pg mp start mvs =
  match mvs with
    | [] -> (start, mp)
    | mv :: rest ->
        (* printf "move: %c\n%!" mv; *)
        let new_mp, new_start = do_move mp start mv in
        let () =
          match visualize with
            | true -> pg new_start (Map.to_alist new_mp)
            | false -> ()
        in
        feed_moves pg new_mp new_start rest

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in

  let parts = s |> String.strip |> split_parts in

  let part1, part2 =
    match parts with
      | [ part1; part2 ] -> (part1, part2)
      | _ -> failwith "bad parts"
  in

  let grid = part1 |> String.split_lines in

  let boundi = List.length grid in
  let boundj = String.length (List.nth_exn grid 0) in
  let pg = print_grid boundi boundj in

  let moves =
    part2 |> Str.global_replace (Str.regexp "\n") "" |> to_char_list
  in

  let mp, start_robot = grid |> map_from_grid in

  (* Map.to_alist mp |> print_grid boundi boundj start_robot; *)
  let robot, mp = feed_moves pg mp start_robot moves in

  let count =
    Map.to_alist mp
    |> List.fold_left
         ~f:(fun a (cell_str, v) ->
           let cell = from_key_tpl cell_str in
           let i, j = cell in
           match v with
             | '[' -> a + (i * 100) + j
             | _ -> a)
         ~init:0
  in

  printf "%d\n" count;
  ()
