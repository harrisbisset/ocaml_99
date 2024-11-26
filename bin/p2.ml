let rec last_two (ls : 'a list) : ('a * 'a) option =
  match ls with
  | [] -> None
  | [ hd; tl ] -> Some (hd, tl)
  | _ :: tl -> last_two tl
;;
