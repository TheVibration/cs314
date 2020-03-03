let rec flatten l =
  (* YOUR CODE HERE *)
  let rec helper acc l1 l2 =
   match l1 with
    | h::t -> helper (h::acc) t l2
    |[] ->
     match l2 with
      | [] -> List.rev acc
      | h::t -> helper acc h t
  in 
  helper [] [] l