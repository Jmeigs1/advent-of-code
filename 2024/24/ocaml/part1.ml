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

let add_to_cache lst =
  let results = Map.Poly.empty in
  let rec process lst results =
    match lst with
      | h :: tail ->
          let k, v =
            h |> String.split ~on:':' |> List.map ~f:String.strip |> get_two
          in
          let n_results = Map.set results ~key:k ~data:(v |> Int.of_string) in
          process tail n_results
      | [] -> results
  in
  process lst results

let parse_gate_line l =
  let p1, output = l |> String.strip |> split_on_re " -> " |> get_two in
  let i1, op, i2 = p1 |> String.split ~on:' ' |> get_three in
  (i1, i2, output, op)

let make_gates lst results =
  let rec process lst results gates =
    match lst with
      | h :: tail ->
          let n_gate = h |> parse_gate_line in
          process tail results (n_gate :: gates)
      | [] -> gates
  in
  process lst results []

let do_op a b op =
  match op with
    | "OR" -> a lor b
    | "XOR" -> a lxor b
    | "AND" -> a land b
    | _ -> failwith "bad op in do_op"

let rec eval_gates results gates unprocessed =
  match gates with
    | [] -> (results, unprocessed)
    | h :: tail -> (
        let i1, i2, output, op = h in
        match Map.mem results i1 && Map.mem results i2 with
          | false -> eval_gates results tail (h :: unprocessed)
          | true ->
              let a = Map.find_exn results i1 in
              let b = Map.find_exn results i2 in
              let v = do_op a b op in
              let n_res = Map.set results ~key:output ~data:v in
              eval_gates n_res tail unprocessed)

let rec loop_gates results = function
  | [] -> results
  | gates ->
      let n_re, up = eval_gates results gates [] in
      loop_gates n_re up

let rec fold_with_idx lst i ~f ~init =
  match lst with
    | [] -> init
    | h :: tail -> fold_with_idx tail (i + 1) ~f ~init:(f init h i)

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let p1, p2 = split_to_parts s in

  let results = add_to_cache (String.split_lines p1) in
  let gates = make_gates (String.split_lines p2) results in

  let results = loop_gates results gates in

  let z_keys =
    results |> Map.keys |> List.filter ~f:(fun k -> String.contains k 'z')
  in

  let result =
    fold_with_idx z_keys 0 ~init:0 ~f:(fun a k i ->
        a + Int.shift_left (Map.find_exn results k) i)
  in

  printf "%d\n" result;
  ()
