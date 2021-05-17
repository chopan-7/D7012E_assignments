%LabP1: Package delivery
% Author: Chonratid Pangdee, chopan-7@student.ltu.se


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STATES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% state(RP, BKP, SKP, PP, HBK, HSK, HP)
% init state: state(r1, r2, r1, r3, hasNot, hasNot, hasNot)
% goal state: state(_, _, _, r2, _, _, _)
%
% Notations:
% RobotPos = RP
% BrassKeyPos = BKP
% SteelKeyPos = SKP
% PackagePos = PP
% HasBrassKey = HBK
% HasSteelKey = HSK
% HasPackagee = HP
%
%%%%%%%%%%%%%%%%%%%%%%%% ACTIONS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pickup, Drop, Walk
% 
%%%%%%%%%%%%%%%%%%%%%%%% RULES OF WALK %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% r1 <-> r2 : state(RP, _, RP, _, _, has, _)		% must have steel_key
% r1 <-> r3 : state(RP, RP, _, _, has, _, _)		% must have brass_key

checkMove(r1, r2, _, has).
checkMove(r1, r3, has, _).
checkMove(r2, r1, _, has).
checkMove(r3, r1, has, _).

%%%%%%%%%%%%%%%%%%%%%%% CHECK STATE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The robot cannot carry more than two items at the same time.
invalid(state(_, _, _, _, has, has, has)).		% invalid state: 3 objects in hands
checkState(State) :- not(invalid(State)).


%%%%%%%%%%%%%%%%%%%%%%%%%% Walk %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

doMove(state(RP, BKP, SKP, PP, HBK, HSK, HP), NewState, Move) :-	% walk between rooms
	checkMove(RP, NewRP, HBK, HSK),
	Move = [walk, NewRP],
	NewState = state(NewRP, BKP, SKP, PP, HBK, HSK, HP),
	checkState(NewState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Pickup %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

doMove(state(PP, BKP, SKP, PP, HBK, HSK, hasNot), NewState, Move) :-	% pickup Package
	Move = [pickup, package],
	NewState = state(PP, BKP, SKP, PP, HBK, HSK, has),
	checkState(NewState).

doMove(state(BKP, BKP, SKP, PP, hasNot, HSK, HP), NewState, Move) :-	% pickup BrassKey
	Move = [pickup, brass_key],
  	NewState = state(BKP, BKP, SKP, PP, has, HSK, HP),
	checkState(NewState).

doMove(state(SKP, BKP, SKP, PP, HBK, hasNot, HP), NewState, Move) :-	% pickup SteelKey
	Move = [pickup, steel_key],
  	NewState = state(SKP, BKP, SKP, PP, HBK, has, HP),
	checkState(NewState).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Drop %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

doMove(state(RP, BKP, SKP, _, HBK, HSK, has), NewState, Move) :-	% drop Package
	Move = [drop, package],
	NewState = state(RP, BKP, SKP, RP, HBK, HSK, hasNot),
	checkState(NewState).

doMove(state(RP, _, SKP, PP, has, HSK, HP), NewState, Move)  :-		% drop BrassKey
	Move = [drop, brass_key],
  	NewState = state(RP, RP, SKP, PP, hasNot, HSK, HP),
	checkState(NewState).

doMove(state(RP, BKP, _, PP, HBK, has, HP), NewState, Move) :-		% drop SteelKey
	Move = [drop, steel_key],
  	NewState = state(RP, BKP, RP, PP, HBK, hasNot, HP),
	checkState(NewState).


%%%%%%%%%%%%%%%%%%%%%%%%% solverR / test %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solverR(state(_,_,_,r2,_,_,_), _, []).	% Solution: the package is dropped in r2.

solverR(State, N, Trace) :-
	N > 0,
	doMove(State, NextState, [Action, Item]),
	Next is N - 1,
	solverR(NextState, Next, TraceDown),
	Trace = [action(Action, Item)|TraceDown].

%%%%%%%%%%%%%%%%%%%%%%%% Tests %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
testSolve(Trace) :-
	solverR(state(r1, r2, r1, r3, hasNot, hasNot, hasNot), 12, Trace),
	print_lst(Trace, 1).

% testSolve(T).

print_lst([], _).
print_lst(Trace, N) :-
	Trace = [X|Y],
	write(N),
	write('. '),
	write(X),nl,
	NextN is N+1,
	print_lst(Y, NextN).


