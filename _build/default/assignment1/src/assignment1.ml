let rec cond_dup l f =
  (* YOUR CODE HERE *)
  match l with
  | [] -> []
  | h::t -> if (f)h then h::[h]@(cond_dup t f) else h::(cond_dup t f)
   (*raise (Failure "Not implemented1")*)

(* assert (cond_dup [3;4;5] (fun x -> x mod 2 = 1) = [3;3;4;5;5]) *)

let rec n_times (f, n, v) =
  (* YOUR CODE HERE *)
  if n<= 0 then v
  else n_times(f,n-1,(f)v)
   (*raise (Failure "Not implemented2")*)

(* assert (n_times((fun x-> x+1), 50, 0) = 50) *)

exception IncorrectRange

let rec range num1 num2 =
  (* YOUR CODE HERE *) 
  if num1 > num2 then raise(IncorrectRange)
  else if num1 = num2 then [num2]
  else num1::range (num1+1) num2
   (*raise (Failure "Not implemented3")*)

  (* assert (range 2 5 = [2;3;4;5]) *)

let rec zipwith f l1 l2 =
  (* YOUR CODE HERE *)
  match (l1,l2) with
      |([],_) -> []
      |(_,[]) -> []
      |(h1::t1,h2::t2) -> f h1 h2::zipwith f t1 t2
   (*raise (Failure "Not implemented4")*)

(* assert (zipwith (+) [1;2;3] [4;5] = [5;7]) *)

let buckets p l =
  (* YOUR CODE HERE *)
   raise (Failure "Not implemented5")

(* assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]) *)
(* assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]) *)
(* assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]]) *)

let rec remove_stutter l =
   match l with
     | [] -> []
     | x::y::tail when x = y -> remove_stutter (y::tail)
     | x::tail -> x::remove_stutter tail

   (*raise (Failure "Not implemented6")*)

(* assert (remove_stutter [1;2;2;3;1;1;1;4;4;2;2] = [1; 2; 3; 1; 4; 2]) *)

let rec flatten l =
  (* YOUR CODE HERE *)
  let rec helper acc l1 l2 =
   match l1 with
    | h::t -> helper (h::acc) t l2
    |[] ->
     match l2 with
      | [] -> List.rev acc
      | h::t -> helper acc h t
  in 
  helper [] [] l
   (*raise (Failure "Not implemented7")*)

(* assert (flatten ([[1;2];[3;4]]) = [1;2;3;4]) *)

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec fold_inorder f acc t =
  (* YOUR CODE HERE *)
  match t with
  | Leaf -> acc
  | Node(l,x,r) ->
     let acc = fold_inorder f acc l in
     let acc = f acc x in
        fold_inorder f acc r
  (* raise (Failure "Not implemented8")*)

(* assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]) *)

let fib_tailrec n =
  (* YOUR CODE HERE *)
  let rec helper prev curr next =
   if prev = n then curr
   else helper (prev+1) (next) (curr+next) in
  helper 0 0 1
  (* raise (Failure "Not implemented9")*)

(* assert (fib_tailrec 50 = 12586269025) *)
