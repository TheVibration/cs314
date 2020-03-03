open Printf

let rec cond_dup lst f =
        match lst with
        |[] -> []
        |h::t -> if (f)h then h::[h]@(cond_dup t f) else h::(cond_dup t f);;

let a = cond_dup [3;4;5;6] (fun x -> x mod 2 = 1)

let () = List.iter(printf "%d\n")a
