open Core
open Re

let get_two = function
  | [ k; v ] -> (k, v)
  | _ -> failwith "bad split get_two"

let get_three = function
  | [ a; b; c ] -> (a, b, c)
  | _ -> failwith "bad split get_two"

let split_on_re r s = s |> Str.split (Str.regexp r)
let split_to_parts s = s |> split_on_re "\n\n" |> get_two
let get_nums = Re.(alt [ rg '0' '9' ] |> rep1 |> compile)
let print_cell lbl (i, j) = Printf.printf "%s - %d:%d\n" lbl i j

let sch_to_set s =
  let s = String.split_lines s in
  let rec process lst i out =
    match lst with
      | [] -> out
      | h :: tail ->
          let n_out =
            String.foldi h ~init:out ~f:(fun j a v ->
                if Char.equal v '#' then Set.add a (i, j) else a)
          in
          process tail (i + 1) n_out
  in
  process s 0 Set.Poly.empty

let sort_schematics schms =
  let rec sort lst keys locks =
    match lst with
      | [] -> (keys, locks)
      | h :: tail ->
          if Char.equal h.[0] '#' then sort tail keys (sch_to_set h :: locks)
          else sort tail (sch_to_set h :: keys) locks
  in
  sort schms [] []

let rec fold_set_lists a set1 = function
  | [] -> a
  | h :: tail ->
      if Set.length (Set.inter set1 h) >= 1 then fold_set_lists a set1 tail
      else fold_set_lists (a + 1) set1 tail

let test_set_list n lst =
  Set.iter (List.nth_exn lst n) ~f:(print_cell "");
  ()

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let schematics = s |> String.strip |> split_on_re "\n\n" in
  let keys, locks = sort_schematics schematics in

  let count =
    List.fold_left keys ~init:0 ~f:(fun a k -> fold_set_lists a k locks)
  in

  printf "%d\n" count;
  ()
