location(1,1,1).
dir(1,east).
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
	XN is X-1,
	XS is X+1,
	YE is Y+1,
	YW is Y-1,
	(
		(not(smelledWumpus(XN, Y)), hasBeenToLocation(XN,Y));
		(not(smelledWumpus(XS, Y)), hasBeenToLocation(XS,Y));
		(not(smelledWumpus(X, YE)), hasBeenToLocation(X,YE));
		(not(smelledWumpus(X, YW)), hasBeenToLocation(X,YW))
	);
	isWall(X,Y);
	hasBeenToLocation(X,Y).

hasNotPit(_,X,Y) :- 
	XN is X-1,
	XS is X+1,
	YE is Y+1,
	YW is Y-1,
	(
		(not(feltPitBreeze(XN, Y)), hasBeenToLocation(XN,Y));
		(not(feltPitBreeze(XS, Y)), hasBeenToLocation(XS,Y));
		(not(feltPitBreeze(X, YE)), hasBeenToLocation(X,YE));
		(not(feltPitBreeze(X, YW)), hasBeenToLocation(X,YW))
	);
	isWall(X,Y). 

hasNotFood(T,X,Y) :- 
	XN is X-1,
	XS is X+1,
	YE is Y+1,
	YW is Y-1,
	(
		(not(smelledFood(XN, Y)), hasBeenToLocation(XN,Y));
		(not(smelledFood(XS, Y)), hasBeenToLocation(XS,Y));
		(not(smelledFood(X, YE)), hasBeenToLocation(X,YE));
		(not(smelledFood(X, YW)), hasBeenToLocation(X,YW))
	);
	isWall(X,Y);
	hasEatenFood(T,X,Y). 

hasPit(_,X,Y) :-
	XN is X-1,
	XS is X+1,
	YE is Y+1,
	YW is Y-1,
	XNN is XN-1,
	XSS is XS+1,
	YEE is YE+1,
	YWW is YW-1,
	(
		(feltPitBreeze(XN, Y), hasNotPit(1, XNN, Y), hasNotPit(1, XN, YE), hasNotPit(1, XN, YW));
		(feltPitBreeze(XS, Y), hasNotPit(1, XSS, Y), hasNotPit(1, XS, YE), hasNotPit(1, XS, YW));
		(feltPitBreeze(X, YE), hasNotPit(1, X, YEE), hasNotPit(1, XN, YE), hasNotPit(1, XS, YE));
		(feltPitBreeze(X, YW), hasNotPit(1, X, YWW), hasNotPit(1, XN, YW), hasNotPit(1, XS, YW))
	).

hasWumpus(_,X,Y) :- 
	XN is X-1,
	XS is X+1,
	YE is Y+1,
	YW is Y-1,
	XNN is XN-1,
	XSS is XS+1,
	YEE is YE+1,
	YWW is YW-1,
	(
		(smelledWumpus(XN, Y), hasNotWumpus(1, XNN, Y), hasNotWumpus(1, XN, YE), hasNotWumpus(1, XN, YW));
		(smelledWumpus(XS, Y), hasNotWumpus(1, XSS, Y), hasNotWumpus(1, XS, YE), hasNotWumpus(1, XS, YW));
		(smelledWumpus(X, YE), hasNotWumpus(1, X, YEE), hasNotWumpus(1, XN, YE), hasNotWumpus(1, XS, YE));
		(smelledWumpus(X, YW), hasNotWumpus(1, X, YWW), hasNotWumpus(1, XN, YW), hasNotWumpus(1, XS, YW))
	). 

hasFood(T,X,Y) :- 
	XN is X-1,
	XS is X+1,
	YE is Y+1,
	YW is Y-1,
	XNN is XN-1,
	XSS is XS+1,
	YEE is YE+1,
	YWW is YW-1,
	(
		(smelledFood(XN, Y), hasNotFood(1, XNN, Y), hasNotFood(1, XN, YE), hasNotFood(1, XN, YW));
		(smelledFood(XS, Y), hasNotFood(1, XSS, Y), hasNotFood(1, XS, YE), hasNotFood(1, XS, YW));
		(smelledFood(X, YE), hasNotFood(1, X, YEE), hasNotFood(1, XN, YE), hasNotFood(1, XS, YE));
		(smelledFood(X, YW), hasNotFood(1, X, YWW), hasNotFood(1, XN, YW), hasNotFood(1, XS, YW))
	),
	not(hasEatenFood(T,X,Y)).




wumpusSmell(-1).
foodSmell(-1).
pitBreeze(-1).
bump(-1).

hasBeenToLocation(X,Y) :-
	hasBeenToLocation(1,X,Y).

hasBeenToLocation(T,X,Y) :-
	T1 is T+1,
	(
		action(T, _);
		wumpusSmell(T);
		foodSmell(T);
		pitBreeze(T);
		bump(T)
	),
	(
		location(T, X, Y);
		location(T1, X, Y);
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

hasEatenFood(T,X, Y) :- 
	action(T0, eat),
	location(T0,X,Y),
	(T0 < T). 


visitedOrWall(X,Y) :-
	isWall(X,Y); hasBeenToLocation(1,X,Y).

smelledWumpus(X,Y) :-
	wumpusSmell(T), location(T,X,Y).

smelledFood(X,Y) :-
	foodSmell(T), location(T,X,Y).

feltPitBreeze(X,Y) :-
	pitBreeze(T), location(T,X,Y).

action(-1, null).

