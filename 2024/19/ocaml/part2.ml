open Core
open Re

let cache = Stdlib.Hashtbl.create 100000000

let split_to_parts s =
  s |> Str.split (Str.regexp "\n\n") |> function
  | [ x; y ] -> (x, y)
  | _ -> failwith "bad split parts"

let get_nums = Re.(alt [ rg '0' '9' ] |> rep1 |> compile)

let join lst sep =
  match lst with
    | h :: t -> List.fold_left ~init:h ~f:(fun a s -> a ^ sep ^ s) t
    | [] -> ""

let match_str ptrn str =
  if String.length ptrn > String.length str then None
  else
    let size = String.length ptrn in
    let sub_str = Stdlib.String.sub str 0 size in
    if String.equal sub_str ptrn then
      let remaining_str =
        Stdlib.String.sub str size (String.length str - size)
      in
      Some remaining_str
    else None

let match_strs ptrns str =
  List.fold_left ptrns ~init:[] ~f:(fun a p ->
      match match_str p str with
        | Some v -> v :: a
        | None -> a)

let count_matches ptrns str =
  let rec count_matches str =
    match String.length str with
      | 0 -> 1
      | _ -> (
          let found = Stdlib.Hashtbl.find_opt cache str in
          match found with
            | Some v -> v
            | None ->
                match_strs ptrns str
                |> List.fold_left ~init:0 ~f:(fun a rem_string ->
                       let v = count_matches rem_string in
                       Stdlib.Hashtbl.add cache rem_string v;
                       a + v))
  in
  count_matches str

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in

  let p1, p2 = split_to_parts s in
  let ptrns = p1 |> String.strip |> Str.split (Str.regexp ", ") in

  let have_towels_for = join ptrns "|" in

  let tst_regex =
    Re.Perl.re ({|^(?:|} ^ have_towels_for ^ {|)+$|}) |> Re.compile
  in

  let lines = p2 |> String.strip |> String.split_lines in

  let test_line l =
    Re.exec_opt tst_regex l |> function
    | Some v -> true
    | None -> false
  in

  let filtered_lines = List.filter lines ~f:test_line in

  let count =
    filtered_lines
    |> List.fold_left ~init:0 ~f:(fun a s -> a + count_matches ptrns s)
  in

  printf "%d\n" count;
  ()
