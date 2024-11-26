let last (ls : 'a list) : 'a option =
  let rec last_item (ls : 'a list) (item : 'a) : 'a =
    match ls with
    | [] -> item
    | head :: tail -> last_item tail head
  in
  match ls with
  | [] -> None
  | head :: tail -> Some (last_item tail head)
;;

let rec last_fixed (ls : 'a list) : 'a option =
  match ls with
  | [] -> None
  | head :: [] -> Some head
  | _ :: tail -> last_fixed tail
;;

let rec last_two (ls : 'a list) : ('a * 'a) option =
  match ls with
  | [] -> None
  | [ hd; md ] -> Some (hd, md)
  | _ :: tl -> last_two tl
;;