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
  let rec process lst results gates gates_map =
    match lst with
      | h :: tail ->
          let ((i1, i2, output, op) as n_gate) = h |> parse_gate_line in
          process tail results (n_gate :: gates)
            (Map.set gates_map ~key:output ~data:(i1, i2, op))
      | [] -> (gates, gates_map)
  in
  process lst results [] Map.Poly.empty

let do_op a b op =
  match op with
    | "OR" -> a lor b
    | "XOR" -> a lxor b
    | "AND" -> a land b
    | _ -> failwith "bad op in do_op"

let maybe_swap swaps v =
  v |> Map.find swaps |> function
  | Some found -> found
  | None -> v

let rec eval_gates results gates swaps unprocessed =
  match gates with
    | [] -> (results, unprocessed)
    | h :: tail -> (
        let i1, i2, output, op = h in
        (* let i1 = maybe_swap swaps i1 in *)
        (* let i2 = maybe_swap swaps i2 in *)
        let output = maybe_swap swaps output in
        match Map.mem results i1 && Map.mem results i2 with
          | false -> eval_gates results tail swaps (h :: unprocessed)
          | true ->
              let a = Map.find_exn results i1 in

              let b = Map.find_exn results i2 in

              let v = do_op a b op in
              let n_res = Map.set results ~key:output ~data:v in
              eval_gates n_res tail swaps unprocessed)

let rec loop_gates results swaps = function
  | [] -> results
  | gates ->
      let n_re, up = eval_gates results gates swaps [] in
      loop_gates n_re swaps up

let rec fold_with_idx lst i ~f ~init =
  match lst with
    | [] -> init
    | h :: tail -> fold_with_idx tail (i + 1) ~f ~init:(f init h i)

let make_result results prefix =
  let keys =
    results |> Map.keys |> List.filter ~f:(fun k -> String.contains k prefix)
  in

  fold_with_idx keys 0 ~init:0 ~f:(fun a k i ->
      a + Int.shift_left (Map.find_exn results k) i)

let rec get_bad_bits n i out =
  match n with
    | 0 -> out
    | _ ->
    match n land 1 with
      | 0 -> get_bad_bits (Int.shift_right n 1) (i + 1) out
      | 1 ->
          let v =
            Int.to_string i |> String.pad_left ~char:'0' ~len:2 |> sprintf "z%s"
          in
          get_bad_bits (Int.shift_right n 1) (i + 1) (v :: out)
      | _ -> failwith "bad land in get_bad_bits"

let get_parents gates_map bad_bits =
  let rec find bits out =
    match bits with
      | [] -> out
      | h :: tail -> (
          let found = Map.find gates_map h in
          match found with
            | Some (a, b, op) ->
                if not (String.equal op "XOR") then find tail (h :: out)
                else find tail out
            | None -> find tail out)
  in
  find bad_bits []

let join lst sep =
  match lst with
    | h :: t -> List.fold_left ~init:h ~f:(fun a s -> a ^ sep ^ s) t
    | [] -> ""

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let p1, p2 = split_to_parts s in

  let results = add_to_cache (String.split_lines p1) in
  let gates, gates_map = make_gates (String.split_lines p2) results in

  let x_in = make_result results 'x' in
  let y_in = make_result results 'y' in
  printf "x_in %o\n" x_in;
  printf "y_in %o\n" y_in;

  let expected = x_in + y_in in
  printf "expected %o\n" expected;

  let add_pair (a, b) lst = (a, b) :: (b, a) :: lst in

  (* literally did this by hand by manaually looking at all the full/half
     adders *)
  let swaps =
    Map.Poly.of_alist_exn
      ([]
      |> add_pair ("jmq", "z06")
      |> add_pair ("gmh", "z13")
      |> add_pair ("qrh", "z38")
      |> add_pair ("cbd", "rqf"))
  in

  (* let swaps = Map.Poly.of_alist_exn [] in *)
  let results = loop_gates results swaps gates in

  let result = make_result results 'z' in

  printf "recieved %o\n" result;
  printf "xor %o\n" (result lxor expected);
  printf "bits %d\n" (Int.floor_log2 expected);
  let bad_bits = get_bad_bits (result lxor expected) 0 [] in

  List.iter bad_bits ~f:(printf "%s\n");

  let output = join (Map.keys swaps) "," in
  printf "%s\n" output;
  ()
