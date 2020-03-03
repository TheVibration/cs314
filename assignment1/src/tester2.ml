let rec flatten l =
  let rec loop res = function
    | [] -> List.rev res
    | h::t -> loop (List.rev_append h res) t
  in
    loop [] l;;