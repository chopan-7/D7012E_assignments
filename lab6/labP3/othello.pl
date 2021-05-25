/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Chonratid Pangdee
%    Student user id  : chopan-7
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
%:- ensure_loaded('play.pl').
:- ensure_loaded('stupid.pl').

% include the help functions from compass.pl
:- [compass].

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 


initialize(State, _) :- initBoard(State).	% initialize board with player 1 to start

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 

winner(State, Plyr) :-
	terminal(State),
	coordinates(C),
	countStones(1, State, C, P1),
	countStones(2, State, C, P2),
	winningPlyr(Plyr, P1, P2).

winningPlyr(1, P1, P2) :- P1 < P2.
winningPlyr(2, P1, P2) :- P1 > P2.

% countStones return the number of stones of given plyr
equal(X, X).
countStones(_, _, [], 0).
countStones(Plyr, State, [First|Coordinates],  N) :-
	get(State, First, Value),
	equal(Plyr, Value) ->					% if..
	countStones(Plyr, State, Coordinates, N1),		% then..
	N is N1 + 1
	;							% else..
	countStones(Plyr, State, Coordinates, N).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 

tie(State) :-
	terminal(State),
	coordinates(C),
	countStones(1, State, C, P1),
	countStones(2, State, C, P2),
	equal(P1,P2).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   

terminal(State) :- 
	moves(1, State, L1), L1 = [n],
	moves(2, State, L2), L2 = [n].

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%   - the list MvList should be sorted by position in order left to right, top to bottom.

moves(1, State, MvList) :-
	coordinates(C),		% Coordinates = whole board
	testmoves(1, State, C, [], Moves),
	sort(Moves, MvList).

moves(2, State, MvList) :-
	coordinates(C),		% Coordinates = whole board
	testmoves(2, State, C, [], Moves),
	sort(Moves, MvList).

% testmoves check for valid placement the board
testmoves(Plyr, State, [Pos|Positions], SoFar, MvList):-
	(validmove(Plyr, State, Pos) ->
	testmoves(Plyr, State, Positions, [Pos|SoFar], MvList);
	testmoves(Plyr, State, Positions, SoFar, MvList)).

testmoves(_,_, [], MvList, MvList).	% return the MvList
testmoves(_,_, [], [], [n]).		% null moves


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%

nextState(1, n, State, State, 2) :- !.
nextState(2, n, State, State, 1) :- !.
nextState(Plyr, Move, State, NewState, NextPlyr) :-
	set(State, NextState, Move, Plyr),			% place plyr stone at Move
	directions(Move, Directions),
	lookforOp(Plyr, State, Move, Directions, OpList),	% look around for opponent
	flipindir(Plyr, NextState, NewState, Move, OpList),	% flip the stones in opponents direction
	opponent(Plyr, NextPlyr).

% flipindir flips stones in all valid directions
flipindir(_, NewState, NewState, _, []).
flipindir(Plyr, State, NewState, Move, [First|OpList]) :-
	movedir(Move, First, Dir),
	flipStones(Plyr, State, NextState, First, Dir),
	flipindir(Plyr, NextState, NewState, Move, OpList).

% flipStones iterates through list of valid directions and change the states
% OpList = list of opponents in different directions
flipStones(Plyr, State, NewState, Start, Dir) :-
	get(State, Start, Val),
	not(equal(Plyr, Val)) ->				
	set(State, NextState, Start, Plyr),
	nextpos(Start, Dir, NextPos),
	flipStones(Plyr, NextState, NewState, NextPos, Dir);
	NewState = State.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

% look all directions for opponent and return true if list of opponents > 0.
validmove(Plyr, State, Proposed) :-
	get(State, Proposed, Pos),				% check current position
	Pos = '.',						% is it empty?
	directions(Proposed, Directions),
	lookforOp(Plyr, State, Proposed, Directions, OpList),
	length(OpList, N),
	N > 0.

% invalid opponent
invalidOpp(_, '.').
invalidOpp(X, X).

% lookforOp check all directions for opponent stone
%  - if found, keep iterating in the direction of found stone to find plyr stone
%  - else, look in next direction.
lookforOp(_, _, _,[],[]).
lookforOp(Plyr, State, Pos, [Start|Directions], Opponents) :-	% if...
	get(State, Start, Op),					
	not(invalidOpp(Plyr, Op)),
	% iterate in direction of opponent to find Plyr		% then..
	movedir(Pos, Start, DirChange),				% change in direction
	iterdir(Plyr, State, Start, DirChange) ->
	lookforOp(Plyr, State, Pos, Directions, NextOp),
	Opponents = [Start|NextOp];
	lookforOp(Plyr, State, Pos, Directions, Opponents).

lookforOp(Plyr, State, Pos, [Start|Directions], Opponents) :-	% else..
	get(State, Start, Op),
	invalidOpp(Plyr, Op),
	lookforOp(Plyr, State, Pos, Directions, Opponents).

outofbound([X,Y]) :- X < 0; X > 5; Y < 0 ; Y > 5.
opponent(1, 2).
opponent(2, 1).

iterdir(Plyr, State, Pos, Dir) :-		% iterate in direction and look for Plyr stone.
	not(outofbound(Pos)),
	get(State, Pos, Valid),
	opponent(Plyr, Op),
	Op = Valid,
	nextpos(Pos, Dir, NextPos),
	iterdir(Plyr, State, NextPos, Dir).

iterdir(Plyr, State, Pos, _) :-		% Stop if plyr stone found.
	not(outofbound(Pos)),
	get(State, Pos, Valid),
	Plyr = Valid.

nextpos([X,Y], [DX, DY], [X2, Y2]) :-	% returns the next coordinate in changing direction
	X2 is X + DX,
	Y2 is Y + DY.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(State,100) :- winner(State,2), !.
h(State,-100) :- winner(State,1), !.
h(State,0) :- tie(State), !.
h(_,0). % otherwise no heuristic guidance used


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(-101).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.
upperBound(101).




% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
