memberL(X,[]) :- false.
memberL(X,[X | T]).
memberL(X,[H | T]) :- memberL(X,T).

reverseL([],[]).
reverseL([H | T],RevX) :- reverseL(T,R), append(R, [H], RevX).


remove_duplicates(L1,L2) :- aux(L1,[],L2).

aux([],Acc,Acc).
aux([H|T],Acc,Rem) :- memberL(H,Acc), aux(T,Acc,Rem).
aux([H|T],Acc,Rem) :- append(Acc,[H],Z),aux(T,Z,Rem).


