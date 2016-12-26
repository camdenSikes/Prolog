%Camden Sikes
%Practice 4

% drop(+N, +L0, -L): L is obtained by removing the first N elements of
% the proper list L0. N must be a non-negative integer.
drop(X,L0,L):-
	(X =:= 0 -> L = L0
	;X > 0 -> [_H|T] = L0, Y is X-1, drop(Y,T,L)
	).

% insert_ord(+RL0, +Element, ?RL):
% RL0 is a proper list of numbers, which is strictly increasing. The
% strictly increasing RL is obtained from RL0 by inserting the number
% Element, if Element is not already in RL0. Otherwise RL = RL0.
insert_ord([], Element, [Element]).
insert_ord([H|T], Element, RL):-
	(Element =:= H -> RL = [H|T]
	;Element < H -> RL = [Element|[H|T]]
	;Element > H -> insert_ord(T, Element, RLT), RL = [H|RLT]
	).

% list_concat(+L1, +L2, -L3): L3 is the concatenation of lists L1 and L2.
list_concat([],L,L).
list_concat([H|T],L0,L):-
	list_concat(T,L0,L1),
	L = [H|L1].

% nth1_1(+N, ?L, ?E): The N-th element of the (possibly open ended) list L
% is E. The head of a list is considered its 1st element.
nth1_1(N,L,E):-
	L = [H|T],
	(N =:= 1 -> E = H
	;N > 1 -> N1 is N-1, nth1_1(N1,T,E)
	).

% nth1_2(?N, +L, ?E): The N-th element of the proper list L is E.
% The head of a list is considered its 1st element.
nth1_2(N,L,E):-
	L = [H|T],
	(H = E, N is 1
	;T \= [], nth1_2(N0,T,E), N is 1+N0
	).

% visible_count(+L, ?N): N is the number of left-visible elements in
% the proper list L of positive integers
visible_count(L,N):-
	visible_count_helper(L,N,0).

% visible_count_helper(+L, ?N, +M): N is the number of left visible
% elements in a tail of a list with a max value M in the head
visible_count_helper([],0,_M).
visible_count_helper([H|T],N,M):-
	(H > M -> visible_count_helper(T,N0,H), N is 1+N0
	;visible_count_helper(T,N,M)
	).