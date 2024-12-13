open Base
open Stdio

let _split_on_whitespace s = s |> Str.split (Str.regexp "[ ]+")

let () =
  let default = "input.txt" in
  let file =
    let args = Sys.get_argv () in
    if phys_equal (Array.length args) 2 then args.(1) else default
  in

  let s = In_channel.read_all file in
  let lines = s |> String.strip |> String.split_on_chars ~on:[ '\n' ] in

  0 |> List.nth lines |> Option.value ~default:"" |> print_endline
