(*let rec remove_stutter l =
         match l with
        | [] -> []
        | x::y::tail when x = y -> remove_stutter(y::tail)
        | x::tail -> x::remove_stutter tail
*)

let rec remove_stutter l =
         match l with
         | [] -> []
         | x::y::tail when x = y -> remove_stutter (y::tail)
         | x::tail -> x::remove_stutter tail

let a = remove_stutter [1;1;2;2;3;4;5]

open Printf

let () = List.iter(printf "%d\n")a
