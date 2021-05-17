% Lab-P2 : Smallest k sets.
% Author: Chonratid Pangdee, chopan-7@student.ltu.se

% Import helpers
:- [list, writer].

% Smallest_k_set :: Input-list -> k
smallest_k_set(L, K) :-
	createSublists(Sublists, L),			% Sublists contains list of index-pairs.
	filter_k_smallest(L, Sublists, K, MinRanges),	% MinRanges = list of k smallest index-pairs.
	getSublists(MinRanges, L, MinSublists),		% MinSublists = lists of sublists.
	createStructures(MinRanges, MinSublists, Rows),
	writeHeader(),
	writeRows(Rows).

% Create list of sublist structures
createStructures([Range|[]], [MinSub|[]], Structures) :-
	sumList(MinSub, Size),
	Range = [I,J],
	Sublist = sublist(Size, I, J, MinSub),		% Sublist structure
	append([Sublist], [], Structures).

createStructures([Range|RT], [MinSub|MT], Structures) :-
	createStructures(RT, MT, NextStructure),
	sumList(MinSub, Size),
	Range = [I,J],
	Sublist = sublist(Size, I, J, MinSub),		% Sublist structure
	append([Sublist], NextStructure, Structures).

% Filter k-smallest sublists
% filter_k_smallest :: List -> Sublists -> k -> smallest-k set
filter_k_smallest(Lst, Subs, 1, X) :- 
	recursive_get_min(X1, Lst, Subs),
	append([], [X1], X).

filter_k_smallest(Lst, Subs, K, X) :-
	K > 1,
	recursive_get_min(Min, Lst, Subs),	% get min index-pair
	findIndex(Min, Subs, Index),		% index of index-pair
	removeAt(Subs, Index, NextSubs),	% remove min index-pair from subs
	K1 is K - 1,
	filter_k_smallest(Lst, NextSubs, K1, NextMin),
	append(NextMin, [Min], X).


% Create list of sublist indecies.
createSublists(X, L) :- 
	length(L, N), 
	sublists(1, N, X).

% List of index-pair.
pair(I, J, Pair) :- append([I], [J], Pair).

% Help function: iterate j-indecies.
iterJ(I, I, Pairs) :- 
	pair(I,I, Pair), 
	append([], [Pair], Pairs).

iterJ(I, J, Pairs) :- 
	J > I, 
	pair(I, J, Pair), 
	Next is J-1, 
	iterJ(I, Next, NextPair), 
	append(NextPair, [Pair], Pairs).

% Help function: iterate i-indecies and return lists of index-pairs.
sublists(I, I, Sublists) :- 
	pair(I, I, Pair), 
	append([],[Pair], Sublists).

sublists(I, N, Sublists) :- 
	I =< N, 
	iterJ(I, N, Sub1), 
	Next is I+1, 
	sublists(Next, N, Sub2), 
	append(Sub1, Sub2, Sublists).

% Get sublists from ranges.
getSublists([H|[]], List, X) :- getSublist(H, List, LastSub), append([], [LastSub], X).
getSublists([H|T], List, Sublists) :-
	getSublists(T, List, NextSub),
	getSublist(H, List, FirstSub),
	append([FirstSub], NextSub, Sublists).


% Get sublist from range.
getSublist(Range, List, Sublist) :-
	elemAt(Range, 1, I),
	elemAt(Range, 2, J),
	I1 is I-1,
	removeFirstK(List, I1, TempList),
	length(List, N),
	K is N-J,
	removeLastK(TempList, K, Sublist).

% Compare size of two sublist and return the min-size sublist.
getMin(X, L1, L2, List) :-
	getSublist(L1, List, Sub1),
	getSublist(L2, List, Sub2),
	sumList(Sub1, S1),
	sumList(Sub2, S2),
	S2 < S1,
	append([], L2, X).

getMin(X, L1, L2, List) :-
	getSublist(L1, List, Sub1),
	getSublist(L2, List, Sub2),
	sumList(Sub1, S1),
	sumList(Sub2, S2),
	S1 < S2,
	append([], L1, X).

getMin(X, L1, L2, List) :-
	getSublist(L1, List, Sub1),
	getSublist(L2, List, Sub2),
	sumList(Sub1, S1),
	sumList(Sub2, S2),
	S1 =:= S2,
	append([], L1, X).


% Recursive get sublist with min-size from list of sublists.
recursive_get_min(X, _, [X]).
recursive_get_min(X, List, [L1| [L2|[]]]) :- getMin(X, L1,L2, List).
recursive_get_min(X, List, [L1|[L2|T]]) :-
	getMin(Min, L1, L2, List),
	append([Min], T, Next),
	recursive_get_min(X, List, Next).


% Find index of index-pair.
% findIndex :: pair -> sublists -> index
findIndex([X1,X2], [[X1, X2]|_], 1).
findIndex([X1,X2], [[Y1,Y2]|T], I) :- 
	X1+X2\=Y1+Y2,
	findIndex([X1, X2], T, NextI),
	I is NextI + 1.

