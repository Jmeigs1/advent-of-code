let _split_on_whitespace s = s |> Str.split (Str.regexp "[ ]+")

let toList s =
  List.init (String.length s) (fun i -> String.sub s i 1 |> int_of_string)

let _debugLst lst =
  lst
  |> List.iter (fun x ->
         print_int x;
         print_string " ")

let doForRange i j fn =
  let rec loopFn i =
    if not (i < j) then ()
    else
      let () = fn i in
      loopFn (i + 1)
  in
  loopFn i

let buildWorkArray lst =
  let open List in
  let size = lst |> fold_left ( + ) 0 in
  let workArr = Array.init size (fun _ -> 0) in

  let rec buildWorkArray inIdx outIdx lstin =
    if length lstin == 0 then workArr
    else
      let h, h2, rest =
        match lstin with
        | [] -> failwith "This shouldnt happen"
        | h :: h2 :: rest -> (h, h2, rest)
        | h :: rest -> (h, 0, rest)
      in
      let value = Int.shift_right inIdx 1 in
      let () = doForRange outIdx (outIdx + h) (fun x -> workArr.(x) <- value) in
      buildWorkArray (inIdx + 2) (outIdx + h + h2) rest
  in
  buildWorkArray 0 0 lst

let processWorkingArray arr =
  let rec loopFn i j count =
    if i > j then count
    else if arr.(i) == 0 then
      if arr.(j) == 0 then loopFn i (j - 1) count
      else loopFn (i + 1) (j - 1) (count + (arr.(j) * i))
    else loopFn (i + 1) j (count + (arr.(i) * i))
  in
  loopFn 0 (Array.length arr - 1) 0

let () =
  let default = "input.txt" in
  let file = if Array.length Sys.argv == 2 then Sys.argv.(1) else default in
  let ic = open_in file in

  let s = really_input_string ic (in_channel_length ic) in
  let blocks = s |> String.trim |> toList in

  let working = blocks |> buildWorkArray in

  working |> processWorkingArray |> string_of_int |> print_endline;
  close_in ic
