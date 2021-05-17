% Test smallest K set lab.
% Author: Chonratid Pangdee, chopan-7@student.ltu.se

:- [smallest_k].

test1_generator(0,R,R).
test1_generator(N,L,R):-
	    T is N*((-1)**N),
	        NextN is N - 1,
		    test1_generator(NextN,[T|L],R).


% test_case(number, list).
test_case(1, [-1, 2, -3, 4, -5]).
test_case(2, [24, -11, -34, 42, -24, 7, -19, 21]).
test_case(3, [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3]).

t1 :-
	test1_generator(100, [], List),
	smallest_k_set(List, 15).

t2 :-
	test_case(2, List),
	smallest_k_set(List, 6).

t3 :-
	test_case(3, List),
	smallest_k_set(List, 8).
