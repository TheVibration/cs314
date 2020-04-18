partition([],[],[]).
partition([H],[H],[]).
partition(L,P,S) :- length(L,N), PLen is div(N,2), SLen is (N - div(N,2)).

partList(Len,[],[]). 
partList(Len,L,[H|T]):- length(H,Len),append(H,LT,L),partList(Len,LT,T).

merge(List, List, []).
merge(List, [], List).

merge([MinList1|RestMerged], [MinList1|RestList1], [MinList2|RestList2]) :- 
    MinList1 =< MinList2,
    merge(RestMerged,RestList1,[MinList2|RestList2]).

merge([MinList2|RestMerged], [MinList1|RestList1], [MinList2|RestList2]) :-
    MinList2 =< MinList1,
    merge(RestMerged,[MinList1|RestList1],RestList2).

mergeSort([], []).
mergeSort([A|[]],[A]).

mergeSort(List, Sorted) :-
    length(List, N),
    FirstLength is //(N, 2),
    SecondLength is N - FirstLength,
    length(FirstUnsorted, FirstLength),
    length(SecondUnsorted, SecondLength),
    append(FirstUnsorted, SecondUnsorted, List),
    mergeSort(FirstUnsorted,FirstSorted),
    mergeSort(SecondUnsorted,SecondSorted),
    merge(FirstSorted, SecondSorted,Sorted).

  
  




