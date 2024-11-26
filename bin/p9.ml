let pack (ls : 'a list) : 'a list list =
  let rec pack' (ls : 'a list) (nl : 'a list) (lls : 'a list list) : 'a list list =
    match ls, nl with
    | [], _ -> lls
    | hd1 :: tl, hd2 :: _ when hd1 = hd2 -> pack' tl (nl @ [ hd1 ]) lls
    | hd :: tl, _ when lls <> [[]] -> pack' tl [ hd ] (lls @ [ nl ])
    | hd :: tl, _ -> pack' tl [ hd ] [ nl ]
  in
  pack' ls [] []
;;

pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e" ]
