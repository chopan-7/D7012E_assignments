% Lab-P2: Smallest k sets.
% Author: Chonratid Pangdee, chopan-7@student.ltu.se

% lists.pl contains help-functions for list operations used in smallest_k.pl.

% Get k-th element from list.
% elemAt :: input-list, k, element
elemAt([X|_], 1, X).
elemAt([_|L], K, X) :- K > 1, K1 is K-1, elemAt(L, K1, X).


% Remove item at index from list.
% removeAt :: input-list -> k -> new-list
removeAt([_|X], 1, X).
removeAt(L, K, X) :-
	length(L, N),
	K2 is N-K+1,
	removeFirstK(L, K, T),
	removeLastK(L, K2, H),
	append(H,T, X).


% Sum of the element of the list.
% sumList :: input-list -> sum
sumList([], 0).
sumList([H|T], Sum) :-
	sumList(T, NextSum),
	Sum is H + NextSum.

% List of integers from I to K.
range(I, I, [I]).
range(I, K, [I|L]) :-
	I < K,
	I1 is I+1,
	range(I1, K, L).

% Remove first K elements from list.
% removeFirstK :: input-list -> k -> new-list
removeFirstK(X, 0, X).
removeFirstK([_|X], 1, X).
removeFirstK([_|L], K, X) :- 
	K > 1, 
	K1 is K-1, 
	removeFirstK(L, K1, X).

% Remove last K element from list.
% removeLastK :: input-list -> k -> new-list
removeLastK(L, K, X) :- 
	reverse(L, L1),			% flip the list 
	removeFirstK(L1, K, L2), 	% remove first-k elements
	reverse(L2, X).

