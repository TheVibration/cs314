let rec range num1 num2 =
  (* YOUR CODE HERE *) 
  if num1 > num2 then raise (Failure "num1>num2")
  else if num1 = num2 then [num2]
  else  num1::range (num1+1) num2

open Printf

let a = range 2 5

let () =  List.iter (printf "%d\n")a
