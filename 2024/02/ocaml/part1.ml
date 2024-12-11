let isGood lst =
  let open List in
  let head = hd lst in
  let tail = tl lst in

  let rec isBad prev lst =
    match lst with
    | h :: (h2 :: _ as rest) ->
        let d1 = h - prev in
        let d2 = h2 - h in
        if d1 * d2 <= 0 || Int.abs d1 > 3 then false else isBad h rest
    | [ h ] ->
        let diff = Int.abs (h - prev) in
        diff <= 3 && diff > 0
    | [] -> true
  in

  isBad head tail

let () =
  let default = "input.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let lines = s |> String.trim |> String.split_on_char '\n' in
  let reports =
    lines
    |> List.map (fun l ->
           l |> String.split_on_char ' ' |> List.map int_of_string)
  in

  reports |> List.filter isGood |> List.length |> Printf.printf "%d\n";
  close_in ic
