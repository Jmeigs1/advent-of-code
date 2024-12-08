let rec _sublist b e l =
  match l with
  | [] -> failwith "sublist"
  | h :: t ->
      let tail = if e = 0 then [] else _sublist (b - 1) (e - 1) t in
      if b > 0 then tail else h :: tail

let rec sum (l : int list) : int =
  match l with [] -> 0 | x :: xs -> x + sum xs
