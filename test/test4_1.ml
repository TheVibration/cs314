let rec zipwidth f l1 l2 =
        match l1 with
            |[] -> []
            |h1::t1 -> match l2 with
                        |[] -> []
                        |h2::t2 -> f h1 h2::zipwidth f t1 t2

let a = zipwidth (+) [1;2;3] [4;5]

open Printf

let () = List.iter(printf "%d\n")a
