open Stdlib

let _  = Random.self_init ()

type term =
  | Constant of string
  | Variable of string
  | Function of string * term list

type head = term
type body = term list

type clause = Fact of head | Rule of head * body

type program = clause list

type goal = term list

let rec string_of_f_list f tl =
  let _, s = List.fold_left (fun (first, s) t ->
    let prefix = if first then "" else s ^ ", " in
    false, prefix ^ (f t)) (true,"") tl
  in
  s

let rec string_of_term t =
  match t with
  | Constant c -> c
  | Variable v -> v
  | Function (f,tl) ->
      f ^ "(" ^ (string_of_f_list string_of_term tl) ^ ")"

let string_of_term_list fl =
  string_of_f_list string_of_term fl

let string_of_goal g =
  "?- " ^ (string_of_term_list g)

let string_of_clause c =
  match c with
  | Fact f -> string_of_term f ^ "."
  | Rule (h,b) -> string_of_term h ^ " :- " ^ (string_of_term_list b) ^ "."

let string_of_program p =
  let rec loop p acc =
    match p with
    | [] -> acc
    | [c] -> acc ^ (string_of_clause c)
    | c::t ->  loop t (acc ^ (string_of_clause c) ^ "\n")
  in loop p ""

let var v = Variable v
let const c = Constant c
let func f l = Function (f,l)
let fact f = Fact f
let rule h b = Rule (h,b)

(* Problem 1 *)
let rec occurs_check v t =
  match t with
    | Constant(x) -> false
    | Variable(x) -> t = v
    | Function(fun_name,l) -> List.fold_left(fun acc x -> if (occurs_check v x) then true else acc) false l

(* Problem 2 *)

module VarSet = Set.Make(struct type t = term let compare = Stdlib.compare end)
(* API Docs for Set : https://caml.inria.fr/pub/docs/manual-ocaml/libref/Set.S.html *)

(*
let rec variables_of_term t =
  let varset_ds = VarSet.empty in
  match t with
  |Constant(x) -> varset_ds
  |Variable(x) -> varset_ds
  |Function(_,lst) -> 
    let rec loop acc l = 
      match lst with
      |[] -> acc
      |x::xs -> 
      match x with
      |Constant(x) -> loop acc xs
      |Variable(x) -> loop (VarSet.add x varset_ds) xs
    in loop t varset_ds lst
*)
let rec variables_of_term t = 
  let varset_ds = VarSet.empty in
  match t with
  |Constant(x) -> VarSet.empty
  |Variable(x) -> VarSet.singleton t
  |Function(_,l) -> List.fold_left(fun acc x -> (VarSet.union (variables_of_term x) (acc))) varset_ds l

let variables_of_clause c =
  let varset_ds = VarSet.empty in
  match c with
  |Fact(f) -> variables_of_term f
  |Rule(h,b) -> VarSet.union (variables_of_term h) (List.fold_left(fun acc x -> (VarSet.union (variables_of_term x) (acc))) varset_ds b)

(* Problem 3 *)

module Substitution = Map.Make(struct type t = term let compare = Stdlib.compare end)
(* See API docs for OCaml Map: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Map.S.html *)

let string_of_substitution s =
  "{" ^ (
    Substitution.fold (
      fun v t s ->
        match v with
        | Variable v -> s ^ "; " ^ v ^ " -> " ^ (string_of_term t)
        | Constant _ -> assert false (* substitution maps a variable to a term *)
        | Function _ -> assert false (* substitution maps a variable to a term *)
    ) s ""
  ) ^ "}"

let rec substitute_in_term s t =
  let acc = [] in
  match t with
  |Constant(x) -> t
  |Variable(x) -> if (Substitution.find_opt t s) = None then t else (Substitution.find t s)
  |Function(fun_name,l) -> 
    Function(fun_name,List.fold_left(fun acc x -> acc@[substitute_in_term s x]) acc l)

let substitute_in_clause s c =
  let acc = [] in
  match c with
  |Fact(f) -> Fact(substitute_in_term s f)
  |Rule(h,b) -> 
    Rule(substitute_in_term s h, List.fold_left(fun acc x -> acc@[substitute_in_term s x]) acc b)

(* Problem 4 *)

let counter = ref 0
let fresh () =
  let c = !counter in
  counter := !counter + 1;
  Variable ("_G" ^ string_of_int c)

let freshen c =
  let vars = variables_of_clause c in
  let s = VarSet.fold (fun v s -> Substitution.add v (fresh()) s) vars Substitution.empty in
  substitute_in_clause s c

(*
let c = (rule (func "p" [var "X"; var "Y"; const "a"]) [func "q" [var "X"; const "b"; const "a"]])
let _ = print_endline (string_of_clause c)
let _ = print_endline (string_of_clause (freshen c))
*)

exception Not_unifiable

let unify t1 t2 =
  let theta = Substitution.empty in
  let rec unify t1 t2 theta =
    let t1 = substitute_in_term theta t1 in
    let t2 = substitute_in_term theta t2 in
    match (t1,t2) with
    |(Variable(x),_) -> if t1 = t2 then theta
      else if (occurs_check t1 t2) then raise(Not_unifiable)
      else Substitution.map (fun value -> substitute_in_term (Substitution.singleton t1 t2) (value)) (Substitution.add t1 t2 theta)
    |(_, Variable(x)) -> if t1 = t2 then theta
      else if (occurs_check t2 t1) then raise(Not_unifiable)
      else Substitution.map (fun value -> substitute_in_term (Substitution.singleton t2 t1) (value)) (Substitution.add t2 t1 theta)
    |(Constant(x),Constant(y)) -> if t1 = t2 then theta else raise(Not_unifiable)
    |(Function(h1,b1), Function(h2,b2)) -> List.fold_left2 (fun theta t1 t2  -> unify t1 t2 theta) theta b1 b2
    | _ -> raise(Not_unifiable)
  in 
  unify t1 t2 theta

(* Problem 5 *)

let nondet_query program goal =
  raise (Failure "Problem 5 Not implemented")

(* Problem Bonus *)

let det_query program goal =
  raise (Failure "Problem Bonus Not implemented")
