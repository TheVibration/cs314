open Syntax
let parse_string = Lambda_parse.parse_string
let string_of_expr = string_of_expr

let r = ref 0

let fresh s =
  let v = !r in
  r := !r + 1;
  s ^ (string_of_int v)

(* Your implementation begins from here *)

let mem e l =
  List.fold_left(fun acc x -> if x=e then true else acc) false l

let remove e l =
  List.fold_left(fun acc x -> if x=e then acc else acc@[x]) [] l

let union l1 l2 =
  let rec remove_stutter l = 
     match l with
     | [] -> []
     | x::y::tail when x=y -> remove_stutter (y::tail)
     | x::tail -> x::remove_stutter tail
  in
  remove_stutter((List.sort String.compare (l1@l2))) 

let add e l =
  (*
  let mem e l = List.fold_left(fun acc x -> if x=e then true else acc) false l
  in
  let union l1 l2 =
    let rec remove_stutter l =
      match l with
      | [] -> []
      | x::y::tail when x=y -> remove_stutter (y::tail)
      | x::tail -> x::remove_stutter tail
    in
    remove_stutter((List.sort String.compare (l1@l2)))
  in
  fun e l -> if (mem (e) (l)) then (List.sort String.compare(l)) else (union l [e])
  *)

  let mem e l = List.fold_left(fun acc x -> if x=e then true else acc) false l
  in
  if (mem (e) (l)) then (List.sort String.compare(l)) else (List.sort String.compare(l@[e]))

let rec free_variables e =
  let union l1 l2 =
    let rec remove_stutter l =
      match l with
      | [] -> []
      | x::y::tail when x=y -> remove_stutter (y::tail)
      | x::tail -> x::remove_stutter tail
    in
    remove_stutter((List.sort String.compare (l1@l2)))
  in
  match e with
  | Var x -> [x]
  | App (e1,e2) -> union (free_variables e1) (free_variables e2)
  | Lam (x,e0) -> remove x (free_variables e0)

let rec substitute expr a b =
  let rec free_variables e =
    let union l1 l2 =
      let rec remove_stutter l =
        match l with
        | [] -> []
        | x::y::tail when x=y -> remove_stutter (y::tail)
        | x::tail -> x::remove_stutter tail
      in
      remove_stutter((List.sort String.compare (l1@l2)))
    in
    match e with
    | Var x -> [x]
    | App (e1,e2) -> union (free_variables e1) (free_variables e2)
    | Lam (x,e0) -> remove x (free_variables e0)
  in
  let mem e l = List.fold_left(fun acc x -> if x=e then true else acc) false l
  in
  match expr with
  | Var x -> if a=x then b else expr
  | App (e1,e2) -> App (substitute e1 a b, substitute e2 a b)
  | Lam (x,e0) -> 
    if a=x then expr
    else if not (mem x (free_variables b)) then Lam (x, substitute e0 a b)
    else
      let z = fresh "z" in 
      let e0' = (substitute (e0) (x) (Var z)) in (Lam (z,(substitute e0' a b)))

let rec reduce_cbv e =
  match e with
  | App (e1,e2) ->
      let e1', boolean = reduce_cbv e1 in
      if boolean then (App (e1',e2),true)
      else let e2', boolean = reduce_cbv e2 in
      if boolean then App(e1,e2'),true
      else (match e1 with
           | Lam (x,e) -> substitute e x e2, true
           | _ -> (e,false))

  | Lam (x,e) -> (Lam(x,e),false)
  | _ -> (e,false) (*if reduction not possible then false*)


let rec reduce_cbn e =
  match e with
  | App (Lam (x,e),e2) -> (substitute e x e2,true) (*b/c reduction is possible*)
  | App (e1,e2) ->
    let e1', boolean = reduce_cbn e1 in
    if boolean then (App (e1',e2),true)
    else e,false
  | Lam (x,e) -> (Lam(x,e),false)
  | _ -> (e,false) (*if reduction not possible then false*)

let rec reduce_normal e =
  match e with
  | App (Lam (x,e),e2) -> (substitute e x e2,true) (*b/c reduction is possible*)
  | App (e1,e2) ->
    let e1', boolean = reduce_normal e1 in
    if boolean then (App (e1',e2),true)
    else let e2', boolean = reduce_normal e2 in 
    if boolean then App (e1,e2'),true
    else e,false
  | Lam (x,e) -> let e', boolean = reduce_normal e in 
    if boolean then Lam (x,e'),true
    else e,false
  | _ -> (e,false) (*if reduction not possible then false*)

(* Your implementation done here *)

(* Debug your code by printing out evaluation results *)
let rec eval log depth reduce expr =
  if depth = 0 then failwith "non-termination?"
  else begin
    let expr', reduced = reduce expr in
    if not reduced then expr else begin
      if log then print_endline ("= " ^ (string_of_expr expr'));
      eval log (depth-1) reduce expr'
    end
  end
(*let eval_cbv = eval true 1000 reduce_cbv*)
(*let eval_cbn = eval true 1000 reduce_cbn*)
let eval_normal = eval true 1000 reduce_normal

(* To debug and observe the evaluation steps of your `reduce_cbv`, `reduce_cbn`
 * or `reduce_normal` implementation, use the following code.
 *
 *let _ = eval_cbv (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *
 *let _ = eval_cbn (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *
 *let _ = eval_normal (parse_string "(\\x.x) ((\\x.x) (\\z.(\\x.x) z))")
 *let _ = print_endline ""
 *)
