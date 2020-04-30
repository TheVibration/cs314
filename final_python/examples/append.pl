append(nil, Q, Q).
append(cons(H, P), Q, cons(H, R)) :- append(P, Q, R).
