let rev (ls : 'a list) : 'a list =
  let rec rev_rec (ls : 'a list) (nl : 'a list) =
    match ls with
    | [] -> nl
    | head :: tail -> rev_rec tail (head :: nl)
  in
  rev_rec ls []
;;
