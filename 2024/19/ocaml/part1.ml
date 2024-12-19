open Core
open Re

let split_to_parts s =
  s |> Str.split (Str.regexp "\n\n") |> function
  | [ x; y ] -> (x, y)
  | _ -> failwith "bad split parts"

let get_nums = Re.(alt [ rg '0' '9' ] |> rep1 |> compile)

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
  let ptrns = p1 |> String.strip |> Str.split (Str.regexp ", ") in

  let have_towels_for = join ptrns "|" in

  let tst_regex =
    Re.Perl.re ({|^(|} ^ have_towels_for ^ {|)+$|}) |> Re.compile
  in

  let lines = p2 |> String.strip |> String.split_lines in

  let test_line a l =
    Re.exec_opt tst_regex l |> function
    | Some v -> 1 + a
    | None -> 0 + a
  in

  let count = lines |> List.fold_left ~init:0 ~f:test_line in
  printf "%d\n" count;
  ()
