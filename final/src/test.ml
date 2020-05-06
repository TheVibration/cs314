let nondet_query program goal =
  let resolvent = goal in
  let rec nondet_query program goal resolvent =
    let a = List.nth resolvent (Random.int(List.length resolvent)) in 
    let a' = List.nth program (Random.int(List.length program)) in  
    let rs = List.fold_left(fun acc x -> if x <> a then acc@[x] else acc) [] resolvent in
    let a = freshen a in
    match a' with
    |Fact(head) ->
      match (unify head a) with
      |exception(Not_unifiable) -> [] 
      |s ->
        let goal' = [substitute_in_term (unify head a) goal] in
        let resolvent' = [substitute_in_term (unify head a) rs] in
        let nondet_query program goal' resolvent' 
    |Rule(head,body) ->
      match (unify head a) with
      |exception(Not_unifiable) -> []
      |s ->
        let goal' = [substitute_in_term (unify head a) goal] in
        let resolvent' = List.fold_left(fun acc x -> acc@[substitute_in_term (unify head a) x] rs body) in
        let nondet_query program goal' resolvent'
    in let loop () = 
      if resolvent = [] then goal else nondet_query program goal resolvent
      
let nondet_query program goal =
    let resolvent = goal in
    let rec nondet_query program goal resolvent =
      match resolvent with
      |[] -> goal
      | resolvent -> 
      let a = List.nth resolvent (Random.int(List.length resolvent)) in
      let a' = List.nth program (Random.int(List.length program)) in
      let rs = List.fold_left(fun acc x -> if x <> a then acc@[x] else acc) [] resolvent in
      let a' = freshen a' in
      match a' with
      |Fact(head) ->
        (match unify head a with
        |exception (Not_unifiable) -> []
        |s ->
        let goal' = List.fold_left(fun acc x -> acc@[substitute_in_term s x]) [] goal in
        let resolvent' = List.fold_left (fun acc x -> acc@[substitute_in_term s x]) [] rs in
        nondet_query program goal' resolvent'
        )
      |Rule(head,body) ->
        (match unify head a with
        |exception (Not_unifiable) -> []
        |s ->
        let goal' = List.fold_left(fun acc x -> acc@[substitute_in_term s x]) [] goal in
        let resolvent' = List.fold_left(fun acc x -> acc@[substitute_in_term s x]) [] (rs@body) in
        nondet_query program goal' resolvent'
        )
      in
      let rec loop () =
        let result = nondet_query program goal resolvent in
        if result = [] then loop ()
        else result
      in
      loop ()

(*original old*)
let nondet_query program goal =
  let resolvent = goal in
  let rec nondet_query program goal resolvent =
    match resolvent with
    |[] -> goal
    in
    let a = List.nth resolvent (Random.int(List.length resolvent)) in 
    let a' = List.nth program (Random.int(List.length program)) in  
    let rs = List.fold_left(fun acc x -> if x <> a then acc@[x] else acc) [] resolvent in
    let a = freshen a in
    match a' with
    |Fact(head) -> 
      (match unify head a with
      |exception (Not_unifiable) -> [] 
      |s ->
      let goal' = List.fold_left(fun acc x -> acc@[substitute_in_term s x]) [] goal in 
      let resolvent' = List.fold_left (fun acc x -> acc@[substitute_in_term s x]) [] rs in
      let nondet_query program goal' resolvent'
      )
    |Rule(head,body) ->
      (match unify head a with
      |exception (Not_unifiable) -> []
      |s ->
      let goal' = List.fold_left(fun acc x -> acc@[substitute_in_term s x]) [] goal in
      let resolvent' = rs@(List.fold_left(fun acc x -> acc@[substitute_in_term s x]) [] body) in
      let nondet_query program goal' resolvent'
      )
    in let loop () = if nondet_query program goal resolvent = [] then goal else nondet_query program goal resolvent
  in nondet_query program goal resolvent