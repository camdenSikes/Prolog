% sublist(+Whole, ?Part, +Before, +Length): Part is a sublist of
% Whole such that there are Before number of elements in Whole
% before Part and the length of Part is Length.
sublist(_L,[],0,0).
sublist([H|LT],[H|T],0,Y0):-
	Y1 is Y0 - 1,
	Y1 >= 0,
	sublist(LT,T,0,Y1).
sublist([_HT|LT],P,X0,Y):-
	X1 is X0 - 1,
	X1 >= 0,
	sublist(LT,P,X1,Y).

% sublist(+Whole, ?Part, ?Before, ?Length, ?After): Part is a
% sublist of Whole such that there are Before number of elements in
% Whole before Part, After number of elements in Whole after Part
% and the length of Part is Length.
sublist([],[],0,0,0).
sublist([_H|T],[],0,0,A):-
	sublist(T,[],0,0,A1),
	A is A1+1.
sublist([H|LT],[H|T],0,L,A):-
	sublist(LT,T,0,L1,A),
	L is L1 + 1.
sublist([_H|T],P,B,L,A):-
	sublist(T,P,B1,L,A),
	B is B1 + 1.

% coeff(Expr, A): The coefficient of x in the
% linear expression Expr is A.
coeff(x,1).
coeff(A,0):-
	number(A).
coeff(A+B,R):-
	coeff(A,R1),
	coeff(B,R2),
	R is R1+R2.
coeff(A-B,R):-
	coeff(A,R1),
	coeff(B,R2),
	R is R1-R2.
coeff(A*B,R):-
	number(A),
	coeff(B,R1),
	R is A*R1.
coeff(A*B,R):-
	number(B),
	\+ number(A),
	coeff(B*A,R).

	
% plateau(L, I, Len): There is a plateau of length Len starting at the
% I-th position of list L.
plateau(L,I,T):-
	plateau_helper(L,I,T,1).

% plateau_helper(L, RI, Len, I): There's a plateau of length Len starting at 
% the RI-th position of a list which starting at the I-th index is L
plateau_helper([H|[H|T]],RI, L, I):-
	plateau_helper2([H|[H|T]],H,L1,T1),
	(RI = I, L = L1
	;I1 is I+L1, plateau_helper(T1,RI,L,I1)
	).
plateau_helper([H|[H1|T]],RI,L,I):-
	H \= H1,
	I1 is I+1,
	plateau_helper([H1|T],RI,L,I1).

% plateau_helper2(L, V, Len, Tail): The length of the plateau of values V
% starting at the beginning of L is Len and the remainder of the list is Tail
plateau_helper2([],_V,0,[]).
plateau_helper2([H|T],V,0,[H|T]):-
	H \= V.
plateau_helper2([V|T],V,L,T1):-
	plateau_helper2(T,V,L1,T1),
	L is L1+1.

% draw(+G, -L): Graph G and line L describe the same graph.
draw(G,L):-
	draw(G,[],L).

% draw(+G, +L, -RL): The egdes from graph G added to line L
% create a line RL.
draw([], L, L).
draw([H|T], L, RL):-
	get_edge([H|T], X-Y, G1),
	(L = [] -> draw(G1, [X-Y], RL)
	;L = [Y-_Z|_T] -> draw(G1, [X-Y|L], RL)
	).

% get_edge(+G, -Edge, -RemGraph): G minus the edge Edge
% or its reverse is RemGraph
get_edge([X-Y|T], X-Y, T).
get_edge([X-Y|T], Y-X, T).
get_edge([H|T], E, RG):-
	get_edge(T, E, RG1), RG = [H|RG1].