let isPair = Str.regexp "[0-9]{2}|[0-9]{2}"
let pairs = ref []

let () = 
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else "input.txt" in
  let ic = open_in file in
  (* let state = "pairs" in *)

  try
    while true do
      let line = input_line ic in
      pairs := if (Str.string_match isPair line 0) then !pairs @ [line] else !pairs;
      print_endline line;
      print_endline (string_of_int (List.length !pairs));
      (* doStuff line; *)
    done 
  with End_of_file -> 
    print_endline (string_of_int (List.length !pairs));
    let f elm = 
      print_endline elm;
      flush stdout in 
    List.iter f !pairs;
    close_in ic
