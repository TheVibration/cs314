 range(S,E,M) :- M >= S, M =< E.

?- range(1,2,2).
?- not(range(1,2,3)).

reverseL([],[]).
reverseL([H | T],RevX) :- reverseL(T,R), append(R, [H], RevX).

?- reverseL([],X).
?- reverseL([1,2,3],X).
?- reverseL([a,b,c],X).

memberL(X,[]) :- false.
memberL(X,[X | T]).
memberL(X,[H | T]) :- memberL(X,T).

?- not(memberL(1, [])).
?- memberL(1,[1,2,3]).
?- not(memberL(4,[1,2,3])).
?- memberL(X, [1,2,3]).

zip([],[],[]).
zip([],B,[]).
zip(A,[],[]).
zip([H | T], [H1 | T1], [H-H1 | Z]) :- zip(T,T1,Z).

?- zip([1,2],[a,b],Z).
?- zip([a,b,c,d], [1,X,y], Z).
?- zip([a,b,c],[1,X,y,z], Z).
?- length(A,2), length(B,2), zip(A, B, [1-a, 2-b]).

insert(X,[H|T],[H|Tail]) :- H < X, insert(X,T,Tail).
insert(X,[X|T],[X,X|T]).
insert(X,[H|T],[X,H|T]) :- H > X.
insert(X,[],[X]).

?- insert(3, [2,4,5], L).
?- insert(3, [1,2,3], L).
?- not(insert(3, [1,2,4], [1,2,3])).
?- insert(3, L, [2,3,4,5]).
?- insert(9, L, [1,3,6,9]).
?- insert(3, L, [1,3,3,5]).

remove_duplicates(L1,L2) :- aux(L1,[],L2).

aux([],Acc,Acc).
aux([H|T],Acc,Rem) :- memberL(H,Acc), aux(T,Acc,Rem).
aux([H|T],Acc,Rem) :- append(Acc,[H],Z),aux(T,Z,Rem).

?- remove_duplicates([1,2,3,4,2,3],X).
?- remove_duplicates([1,4,5,4,2,7,5,1,3],X).
?- remove_duplicates([], X).

intersectionL([],[],[]).
intersectionL([],L2,[]).
intersectionL(L1,[],[]).

intersectionL([H|T],L2,[H|Tail]) :- memberL(H,L2),intersectionL(T,L2, Tail).
intersectionL([H|T],L2,Z) :- intersectionL(T,L2,Z).

?- intersectionL([1,2,3,4],[1,3,5,6],[1,3]).
?- intersectionL([1,2,3,4],[1,3,5,6],X).
?- intersectionL([1,2,3],[4,3],[3]).

prefix(P,L) :- append(P,_,L).
suffix(S,L) :- append(_,S,L).

partition([],[],[]).
partition([H],[H],[]).
partition(L,P,S) :- length(L,N), PLen is div(N,2), SLen is (N - div(N,2)), length(P,PLen), length(S, SLen), prefix(P,L), suffix(S,L).

?- partition([a],[a],[]).
?- partition([1,2,3],[1],[2,3]).
?- partition([a,b,c,d],X,Y).

merge([], List, List).
merge(List, [], List).

merge([MinList1|RestList1], [MinList2|RestList2], [MinList1|RestMerged]) :- 
    MinList1 =< MinList2,
    merge(RestList1,[MinList2|RestList2],RestMerged).

merge([MinList1|RestList1], [MinList2|RestList2], [MinList2|RestMerged]) :-
    MinList2 =< MinList1,
    merge([MinList1|RestList1],RestList2,RestMerged).

?- merge([],[1],[1]).
?- merge([1],[],[1]).
?- merge([1,3,5],[2,4,6],X).

mergesort([], []).
mergesort([H|[]],[H]).
mergesort(List, Sorted) :-
    length(List, N),
    FirstLength is //(N, 2),
    SecondLength is N - FirstLength,
    length(FirstUnsorted, FirstLength),
    length(SecondUnsorted, SecondLength),
    append(FirstUnsorted, SecondUnsorted, List),
    mergesort(FirstUnsorted,FirstSorted),
    mergesort(SecondUnsorted,SecondSorted),
    merge(FirstSorted, SecondSorted,Sorted).

?- mergesort([3,2,1],X).
?- mergesort([1,2,3],Y).
?- mergesort([],Z).
