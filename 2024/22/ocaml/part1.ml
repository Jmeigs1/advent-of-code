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

let get_nth fn limit (start : int) =
  let rec get_nth i v =
    if i >= limit then v
    else
      let n_val = fn v in
      get_nth (i + 1) n_val
  in
  get_nth 0 start

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines =
    s |> String.strip |> String.split_lines |> List.map ~f:Int.of_string
  in

  let count =
    lines |> List.fold_left ~init:0 ~f:(fun a v -> a + get_nth get_next 2000 v)
  in

  printf "%d\n" count;
  ()
