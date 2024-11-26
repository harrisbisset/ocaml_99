let length (ls : 'a list) =
  let rec len (i : int) (ls : 'a list) : int =
    match ls with
    | [] -> i
    | _ :: tl -> len (i + 1) tl
  in
  len 0 ls
;;
