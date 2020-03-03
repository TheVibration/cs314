let succ x = x+1;;

let _ = Printf.printf "The Result is %d.\n" (succ 0)
let _ = assert (succ 0 = 1)
