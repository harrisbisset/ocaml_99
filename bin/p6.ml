let is_palindrome (ls : 'a list) : bool =
  let rec is_palindrome' (ls : 'a list) (rl : 'a list) : bool =
    match ls, rl with
    | [], [] -> true
    | _, [] -> false
    | [], _ -> false
    | hd1 :: tl1, hd2 :: tl2 -> if hd1 = hd2 then is_palindrome' tl1 tl2 else false
  in
  is_palindrome' ls (P5.rev ls)
;;

let is_palindrome_fixed (ls : 'a list) : bool = ls <> [] && ls = P5.rev ls
