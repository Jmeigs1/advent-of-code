let rec shift idx lst =
  if idx <= 0 then lst
  else match lst with _ :: t -> shift (idx - 1) t | [] -> failwith "asdf"

let isBadIdx lst =
  let open List in
  let head = hd lst in
  let tail = tl lst in

  let rec isBad prev lst i =
    match lst with
    | h :: (h2 :: _ as rest) ->
        let d1 = h - prev in
        let d2 = h2 - h in
        if d1 * d2 <= 0 || Int.abs d1 > 3 then i else isBad h rest (i + 1)
    | [ h ] ->
        let diff = Int.abs (h - prev) in
        if diff <= 3 && diff > 0 then -1 else i
    | [] -> -1
  in
  isBad head tail 1

let canFixBad (lst, idx) =
  let open List in
  let lst = shift (idx - 2) lst in
  let head = hd lst in
  let tail = tl lst in

  let rec canFixBad front current back i =
    if i > 3 then false
    else
      let working = front @ back in
      if isBadIdx working == -1 then true
      else if List.length back == 0 then false
      else
        let h = hd back in
        let t = tl back in

        canFixBad (front @ [ current ]) h t (i + 1)
  in

  canFixBad [] head tail 0

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

  let isBad (_, x) = x != -1 in

  let count1 =
    reports
    |> List.map (fun r -> (r, isBadIdx r))
    |> List.filter isBad |> List.filter canFixBad |> List.length
  in

  let count2 =
    reports
    |> List.map (fun r -> (r, isBadIdx r))
    |> List.map isBad
    |> List.filter (Bool.equal false)
    |> List.length
  in

  count1 + count2 |> Printf.printf "%d\n";

  close_in ic
