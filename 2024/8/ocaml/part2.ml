let default = "input.txt" in
let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
let ic = open_in file in

let _split_on_whitespace s = s |> Str.split (Str.regexp "[ ]+") in

let s = really_input_string ic (in_channel_length ic) in

let lines = s |> String.trim |> String.split_on_char '\n' in

1 |> List.nth lines |> print_endline;

close_in ic
