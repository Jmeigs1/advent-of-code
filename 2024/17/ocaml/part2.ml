open Core

let _split_on_whitespace s = s |> Str.split (Str.regexp "[ ]+")

let () =
  let default = "input.txt" in
  let args = Sys.get_argv () in
  let file = if Int.equal (Array.length args) 2 then args.(1) else default in

  let s = In_channel.read_all file in
  let lines = s |> String.strip |> String.split_on_chars ~on:[ '\n' ] in

  1 |> List.nth_exn lines |> print_endline
