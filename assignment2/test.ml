let remove e l = List.fold_left(fun acc x -> if x = e then acc else x::acc) [] l

let returned = remove "b" ["a";"b";"c";"d"]

open Printf

let () = List.iter(printf "%S\n)returned
