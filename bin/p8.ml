type 'a last_list =
  { last : 'a
  ; ls : 'a list
  }

let compress (ls : 'a list) : 'a list =
  let rec compress' (ls : 'a list) (nl : 'a list) : 'a list =
    match ls, nl with
    | [], _ -> List.rev nl
    | hd :: tl, hd2 :: _ when hd = hd2 -> compress' tl nl
    | hd :: tl, _ -> compress' tl (hd :: nl)
  in
  compress' ls []
;;

compress [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
