let default = "input.txt" in
let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
let ic = open_in file in

let s = really_input_string ic (in_channel_length ic) in

print_endline (String.trim s);

close_in ic
