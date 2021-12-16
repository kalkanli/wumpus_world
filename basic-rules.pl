

action(1,forward).
action(2,forward).
action(3,forward).
action(4,counterClockWise).
action(5,forward).
action(6,counterClockWise).
action(7,forward).
action(8,forward).
action(9,forward).
action(10,counterClockWise).
action(11,forward).
action(12,forward).
action(13,forward).
action(14,forward).
action(15,forward).
action(16,forward).
action(17,forward).
action(18,forward).
action(19,counterClockWise).
action(20,forward).
action(21,counterClockWise).
action(22,forward).
pitBreeze(10).
pitBreeze(11).
bump(19).
wumpusSmell(21).
wumpusSmell(22).

location(1,4,2).
location(T1,R,C) :-
  T0  is T1 - 1,
  RN  is R - 1,
  RS  is R + 1,
  CW  is C - 1,
  CE  is C + 1,
  (
    ((action(T0,eat); action(T0,clockWise); action(T0,counterClockWise)), location(T0,R,C));
    ((action(T0,attack);action(T0,forward)), bump(T1), location(T0,R,C));
    ((action(T0,attack);action(T0,forward)), dir(T0,north), not(bump(T1)), location(T0,RS,C));
    ((action(T0,attack);action(T0,forward)), dir(T0,south), not(bump(T1)), location(T0,RN,C));
    ((action(T0,attack);action(T0,forward)), dir(T0,west),  not(bump(T1)), location(T0,R,CE));
    ((action(T0,attack);action(T0,forward)), dir(T0,east),  not(bump(T1)), location(T0,R,CW))
  ).

dir(1,north).
dir(T1,north) :-
  T0 is T1 - 1,
		(
				((action(T0,eat); action(T0,attack); action(T0,forward)), dir(T0,north) );
				(action(T0,clockWise)       , dir(T0,west));
				(action(T0,counterClockWise), dir(T0,east))
		).

dir(T1,east) :-
  T0 is T1 - 1,
		(
				((action(T0,eat);action(T0,attack);action(T0,forward)), dir(T0,east));
				(action(T0,clockWise)       , dir(T0,north));
				(action(T0,counterClockWise), dir(T0,south))
		).

dir(T1,south) :-
  T0 is T1 - 1,
		(
				((action(T0,eat);action(T0,attack);action(T0,forward)), dir(T0,south));
				(action(T0,clockWise)       , dir(T0,east));
				(action(T0,counterClockWise), dir(T0,west))
		).

dir(T1,west) :-
  T0 is T1 - 1,
		(
				((action(T0,eat);action(T0,attack);action(T0,forward)), dir(T0,west) );
				(action(T0,clockWise)       , dir(T0,south));
				(action(T0,counterClockWise), dir(T0,north))
		).


/* Wall variables related */
% isWall(T,R,C) :- 
% 	isWall(R,C).

isWall(X,Y) :-
	(
		bump(T),
		T0 is T-1,
		XS is X+1,
		XN is X-1,
		YE is Y+1,
		YW is Y-1,
		action(T0, forward),
		(
			(location(T0, XN, Y), location(T, XN, Y), dir(T0, south));
			(location(T0, XS, Y), location(T, XS, Y), dir(T0, north));
			(location(T0, X, YE), location(T, X, YE), dir(T0, west));
			(location(T0, X, YW), location(T, X, YW), dir(T0, east))
		)
	);
	(X =:= 0; Y =:= 0).

hasNotWumpus(_,X,Y) :- 
	(
		wumpusSmell(T0),
		XN is X-1,
		XS is X+1,
		YE is Y+1,
		YW is Y-1,
		not(
			location(T0, XN, Y);
			location(T0, XS, Y);
			location(T0, X, YE);
			location(T0, X, YW)
		)
	).

hasNotPit(_,X,Y) :- 
	(
		pitBreeze(T0),
		XN is X-1,
		XS is X+1,
		YE is Y+1,
		YW is Y-1,
		not(
			location(T0, XN, Y);
			location(T0, XS, Y);
			location(T0, X, YE);
			location(T0, X, YW)
		)
	).

% hasNotPit(-1,X,Y).


% isClear(T,R,C) :-     % change in Q1
% 		hasNotWumpus(T,R,C), 
% 		hasNotPit(T,R,C),
% 		not(isWall(R,C)).

% bump(-1).

% hasNotWumpus(T,X,Y).   % change in Q2
% hasNotPit(-1,X,Y).    % change in Q2

% hasPit(-1,X,Y).       % change in Q3
% hasWumpus(-1,X,Y).     % change in Q3
% hasDeadWumpus(-1,X,Y). % change in Q3

% hasFood(-1,X,Y).      % change in Q4
% hasNotFood(T,X,Y).    % change in Q4

% isWall(-1,-1).
% isWall(-1,-1,-1).



