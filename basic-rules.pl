action(1,forward).
action(2,counterClockWise).
action(3,counterClockWise).
action(4,forward).
action(5,counterClockWise).
action(6,forward).
action(7,counterClockWise).
action(8,forward).
action(9,forward).
action(10,counterClockWise).
action(11,attack).
wumpusSmell(2).
wumpusSmell(3).
wumpusSmell(4).
pitBreeze(7).
pitBreeze(8).
foodSmell(10).
wumpusSmell(10).
foodSmell(11).
wumpusSmell(11).

bump(-1).

location(1,1,1).
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

dir(1,east).
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


hasPit(_,X,Y) :- 
	(
		pitBreeze(T0),
		XN is X-1,
		XS is X+1,
		YE is Y+1,
		YW is Y-1,
		(
			location(T0, XN, Y);
			location(T0, XS, Y);
			location(T0, X, YE);
			location(T0, X, YW)
		),
		(
			not(isWall(X, Y)),
			not(hasBeenToLocation(X,Y))
		)
	).

hasWumpus(_,X,Y) :- 
	(
		wumpusSmell(T0),
		XN is X-1,
		XS is X+1,
		YE is Y+1,
		YW is Y-1,
		(
			location(T0, XN, Y);
			location(T0, XS, Y);
			location(T0, X, YE);
			location(T0, X, YW)
		),
		(
			not(isWall(X, Y)),
			not(hasBeenToLocation(X,Y))
		)
	).

hasBeenToLocation(X,Y) :-
	hasBeenToLocation(1,X,Y).

hasBeenToLocation(T,X,Y) :-
	T1 is T+1,
	action(T, _),
	(
		location(T, X, Y);
		hasBeenToLocation(T1,X,Y)
	).

hasDeadWumpus(_,X,Y) :-
	XN is X-1,
	XS is X+1,
	YE is Y+1,
	YW is Y-1,
	hasWumpus(_,X,Y),
	action(T0, attack),
	(
		(dir(T0, east), location(T0, X, YW));
		(dir(T0, west), location(T0, X, YE));
		(dir(T0, north), location(T0, XS, Y));
		(dir(T0, south), location(T0, XN, Y))
	).



% hasFood(-1,X,Y).      % change in Q4
% hasNotFood(T,X,Y).    % change in Q4



