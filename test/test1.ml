open Printf

let rec cond_dup lst f =
        match lst with
        |[] -> []
        |h::t -> if (f)h then (h::[h])@(cond_dup t f) else h::(cond_dup t f);;


let a = cond_dup [3;4;5] (fun x-> x mod 2 = 1)
let () = List.iter (printf "%d ")a

let b = cond_dup [1;2;3;4;5;6;7;8;9] (fun x-> x mod 2 = 1)
let () = List.iter (printf "%d ")b
