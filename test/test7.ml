let rec flatten l =
    let rec aux acc list1 list2 =
        match list1 with
         | x :: tail -> aux (x::acc) tail list2
         | [] ->
           match list2 with
             | [] -> List.rev acc
             | x :: tail -> aux acc x tail
    in
    aux [] [] l

let a = flatten [[0];[1;2];[3;4];[5;6;7;8];[9]]

open Printf

let () = List.iter (printf "%d\n")a
