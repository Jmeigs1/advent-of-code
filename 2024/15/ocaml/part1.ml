open Core
open Re

let split_parts s = s |> Str.split (Str.regexp "\n\n")
let to_char_list s = List.init (String.length s) ~f:(String.get s)

(* --- tpl --- *)
let add_tpl (i, j) (h, k) = (i + h, j + k)
let eq_tpl (i, j) (h, k) = equal i h && equal j k
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

let print_grid boundi boundj lst =
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

  let rec print_loop n =
    if Int.equal n (Array.length arr) then ()
    else
      let () = printf "%s\n" arr.(n) in
      print_loop (n + 1)
  in
  add_points lst;
  print_loop 0

let map_from_grid lst =
  let start_mp = Map.empty (module String) in
  let start_robot = ref (0, 0) in

  let rec process_line mp line i j =
    if Int.equal j (String.length line) then mp
    else
      match line.[j] with
        | '#' ->
            let new_mp = Map.set mp ~key:(key_tpl (i, j)) ~data:'#' in
            process_line new_mp line i (j + 1)
        | 'O' ->
            let new_mp = Map.set mp ~key:(key_tpl (i, j)) ~data:'O' in
            process_line new_mp line i (j + 1)
        | '@' ->
            start_robot := (i, j);
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

let get_shift_lst mp start dir =
  let rec find lst start =
    let next = add_tpl start dir in
    let next_val = get_mp_tpl mp (key_tpl next) in

    match next_val with
      | None -> Some lst
      | Some v ->
      match v with
        | '#' -> None
        | v ->
            let new_list = (next, v) :: lst in
            find new_list (add_tpl start dir)
  in

  find [] start

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
    | Some v when Char.equal v 'O' -> (
        let shift_list = get_shift_lst mp start dir in
        match shift_list with
          | None -> (mp, start)
          | Some v ->
              let new_mp = proces_shift_list mp dir v in
              (new_mp, next_start))
    | Some _ -> failwith "bad next_cell in do_move"

let rec feed_moves mp start mvs =
  match mvs with
    | [] -> mp
    | mv :: rest ->
        let new_mp, new_start = do_move mp start mv in
        feed_moves new_mp new_start rest

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
  let moves =
    part2 |> Str.global_replace (Str.regexp "\n") "" |> to_char_list
  in

  let mp, start_robot = grid |> map_from_grid in

  let mp = feed_moves mp start_robot moves in

  let count =
    Map.to_alist mp
    |> List.fold_left
         ~f:(fun a (cell_str, v) ->
           let cell = from_key_tpl cell_str in
           let i, j = cell in
           match v with
             | 'O' -> a + (i * 100) + j
             | _ -> a)
         ~init:0
  in

  printf "%d\n" count;
  ()
