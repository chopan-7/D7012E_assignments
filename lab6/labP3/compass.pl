% Author: Chonratid Pangdee, chopan-7@student.ltu.se
% Compass.pl contains help functions for othello.pl
%

% list all possible directions from start position
directions([0,0], [[0, 1], [1, 1], [1, 0]]) :- !.	% from top-left
directions([5,0], [[4, 0], [4, 1], [5, 1]]) :- !.	% from top-right
directions([0,5], [[0, 4], [1, 4], [1, 5]]) :- !.	% from bottom-left
directions([5,5], [[4, 5], [4, 4], [5, 4]]) :- !.	% from botton-right

directions([0, Y], [[0, Y1], [0, Y2], [1, Y], [1, Y1], [1, Y2]]) :-	% from first column
	Y1 is Y+1,
	Y2 is Y-1.
directions([X, 0], [[X2, 0], [X, 1], [X1, 0], [X2, 1], [X1, 1]]) :-	% from first row
	X1 is X+1,
	X2 is X-1.
directions([5, Y], [[5, Y2], [4, Y], [5, Y1], [4, Y1], [4, Y2]]) :-	% from last column
	Y1 is Y+1,
	Y2 is Y-1.
directions([X, 5], [[X2, 5], [X, 4], [X1, 5], [X1, 4], [X2, 4]]) :-	% from last row
	X1 is X+1,
	X2 is X-1.
directions([X,Y], L) :- 
	X > 0, Y < 5,
	X1 is X+1,
	Y1 is Y+1,
	X2 is X-1,
	Y2 is Y-1,
	L = [ 
		[X, Y2],		% north
		[X1, Y2],		% northeast
		[X1, Y],		% east
		[X1, Y1],		% southeast
		[X, Y1],		% south
		[X2, Y1],		% southwest
		[X2, Y],		% west
		[X2, Y2]		% northwest
	      ].

% movedir calculate the change in direction.
movedir([X1,Y1], [X2, Y2], Next) :-
	X is X2-X1,
	Y is Y2-Y1,
	Next = [X,Y].


coordinates(X) :- iterrows([5,5], X).

% Iterrows and Itercols are help functions that returns
% a list of all coordinates of the board.
iterrows([X,0], MvList) :- itercols([X, 0], MvList).
iterrows([X,Y], MvList) :-
	Y > 0,
	Y1 is Y-1,
	iterrows([X,Y1], L1),
	itercols([X, Y], L2),
	append(L1, L2, MvList).

itercols([0,Y], [[0,Y]]).
itercols([X,Y], MvList) :-
	X >= 0,
	X1 is X-1,
	itercols([X1, Y], L),
	MvList = [[X,Y]|L].
