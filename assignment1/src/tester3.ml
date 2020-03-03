let buckets p l =
 let rec pointer f2 t2 acc2 =
  match acc2 with
  |[] -> [[t2]]
  |h3::t3 -> match t3 with
              |[] -> [[t2]]
              |h4::_ -> if p h3 h4 then (t2::h3)::t3 
                        else h3::(pointer p t2 t3)
 in
 let rec go_through f1 t1 acc1 =
  match t1 with
   |[] -> acc1
   |h1::t1 -> go_through p t1 (pointer p h1 acc1)
 in
 match l with
  |[]->[]
  |h::t -> go_through p t [[h]]
