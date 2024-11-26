let rec at (k : int) (ls : 'a list) : 'a option =
  if k < 1
  then None
  else (
    match ls with
    | [] -> None
    | head :: tail -> if k = 1 then Some head else at (k - 1) tail)
;;

let rec at_fixed (k : int) (ls : 'a list) : 'a option =
  match ls with
  | [] -> None
  | hd :: _ when k = 1 -> Some hd
  | _ :: tl -> if k < 1 then None else at_fixed (k - 1) tl
;;

at_fixed 3 [ "a"; "b"; "c"; "d"; "e" ]
