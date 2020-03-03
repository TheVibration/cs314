let rec zipwith f l1 l2 =
    match (l1,l2) with
        |([],_) -> []
        |(_,[]) -> []
        |(h1::t1,h2::t2) -> f h1 h2::zipwith f t1 t2

let a = zipwith (+) [1;2;3] [4;5]

open Printf

let () = List.iter(printf "%d\n")a
