type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten (nodes : 'a node list) : 'a list =
  let rec flatten' (nodes : 'a node list) (ls : 'a list) : 'a list =
    match nodes with
    | [] -> ls
    | One hd :: tl -> flatten' tl (ls @ [ hd ])
    | Many hd :: tl -> flatten' tl (flatten' hd ls)
  in
  flatten' nodes []
;;

flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
