% Lab-P2 : Smallest k sets.
% Author: Chonratid Pangdee, chopan-7@student.ltu.se
%
% writer.pl contains helper functions print functions for smallest_k.pl.

% Import helpers
:- [list].

writeHeader() :- 
	write('size\ti\tj\tsublist'),	% 	size	i	j	sublist
	nl.

writeRow(Size, I, J, Sub) :- 		%	-3	2	4	[1, -2, -1, 4]
	write(Size),
	write('\t'),
	write(I), write('\t'), write(J),
	write('\t'),
	write(Sub),nl.

writeRows([]).
writeRows([Sub|T]) :-
	writeRows(T),
	Sub = sublist(Size, I, J, MinSub),
	writeRow(Size, I, J, MinSub).
	

