% SDT Assignment 1: Sudoku
% By Camden Sikes

:- use_module(library(lists)).

% consistent(+SGrid): For all areas of the Sudoku grid SGrid it holds
% that all positive integers in the area are distinct.
consistent(SGrid):-
	valid_rows(SGrid),
	valid_columns(SGrid),
	valid_boxes(SGrid).


% valid_rows(+M): true if M has no repeated positive integers in columns
valid_rows([]).
valid_rows([H|T]):-
	delete(H, 0, L),
	no_repeats(L),
	valid_rows(T).

% valid_columns(+M): true if M has no repeated positive integers in columns
valid_columns(M):-
	transpose(M,MT),
	valid_rows(MT).

% valid_boxes(+M): True if there are no repeated positive integers in boxes
valid_boxes(M):-
	length(M, N),
	NB is integer(sqrt(N)),
	valid_boxes(M, NB, NB).

% valid_boxes(+M): True if there are no repeated positive integers in boxes
valid_boxes(_M, _NR, 0).
valid_boxes([[]|T], NR, NB):-
	delete(T, [], R),
	NB1 is NB - 1,
	valid_boxes(R, NR, NB1).
valid_boxes([H|T], NR, NB):-
	H \= [],
	frstnmnz([H|T], NR, NR, L, M),
	no_repeats(L),
	valid_boxes(M, NR, NB).

% no_repeats(+L): true if there are no repeated numbers in L
no_repeats([]).
no_repeats([H|T]):-
	\+ member(H, T),
	no_repeats(T).

% frstnmnz(+Matrix, +NR, +N, ?RL, -Rest): RL is all the non-zero elements 
% of he first N elements of the first NR rows of Matrix.
% Rest is the matrix without the first N elements from the first NR rows
frstnmnz(M, NR, N, RL, R):-
	NE is NR*N,
	frstnmnz(M, NE, N, [], RL, [], R).

% frstnmnz(+Matrix, +NE, +N, +RL0, ?RL, +R0, -Rest): RL is an unordered
% list of non-zero elements from NE elements of Matrix taking N from each
% row. (If NE is not a multiple of N, takes fewer from last row). 
% Rest is R0 appended to (M without the NE elements considered)
frstnmnz(M, 0, _N, RL0, RL0, R0, R):-
	append(R0, M, R).
frstnmnz([[H|T]|MT], NE, N, RL0, RL, R0, R):-
	NE > 0,
	NE1 is NE-1,
	(   N = 1 -> M = MT, append(R0, [T], R1)
	;   1 is NE rem N -> M = MT, append(R0, [T], R1)
	;   M = [T|MT], R1 = R0
	),
	(   H = 0 -> RL1 = RL0
	;   RL1 = [H|RL0]
	),
	frstnmnz(M, NE1, N, RL1, RL, R1, R).
		



%-------------------------------------------------------------------------
%-------------------------------------------------------------------------




% sudoku0(+Grid0, ?Grid): Grid is a complete refinement
% of the Sudoku grid Grid0
sudoku0(G0, G):-
	fill(G0, G),
	consistent(G).

% fill(+Grid0, ?Grid): Grid is Grid0 with all zeros replaced with
% integers in the range for the grid.
fill(G0, G):-
	length(G0, N),
	fill(G0, N, _AL, G).

% fill(+Grid0, +N, +AL, ?Grid): Grid is Grid0 with all zeros replaced with
% integers =< N appended to AL.
fill([], _N, _AL, []).
fill([[]|T], N, AL, G):-
	AL = [],
	fill(T, N, _AL, G).
fill([[H|T]|T1], N, AL, G):-
	(   H = 0 -> enumerate(N, V)
	;   V = H
	),
	AL = [V|AL1],
	(   length([H|T], N) -> G = [AL|G1]
	;   G1 = G
	),
	fill([T|T1], N, AL1, G1).

% enumerate(+N, ?V): V is a positive integer <= N
enumerate(N, N).
enumerate(N, V):-
	(   N > 1 -> N1 is N - 1,
	       enumerate(N1, V)
	).



%-------------------------------------------------------------------------
%-------------------------------------------------------------------------



%Other approaches for some of the predicates


% valid_columns_1(+M): true if M has no repeated positive ints in columns
valid_columns_1(M):-
	length(M, N),
	valid_columns(M, N).

% valid_columns_1(+M, +N): true if M has no repeated positive integers
% in columns L or lower
valid_columns_1(_M, 0).
valid_columns_1(M, N):-
	N > 0,
	nth_column(M, N, L),
	no_repeats(L),
	N1 is N - 1,
	valid_columns_1(M, N1).

% nth_column(+Matrix, +N, ?List): List is the Nth column of Matrix
nth_column(M, N, L):-
	nth_column(M, N, [], L).

% nth_column(+Matrix, +N, +L0, ?List): List is the Nth column of Matrix
% appended to L0, with 0s omitted
nth_column([], _N, L, L).
nth_column([H|T], N, L0, L):-
	nth1(N, H, E),
	(   E = 0 -> L1 = L0
	;   L1 = [E|L0]
	),
	nth_column(T, N, L1, L).


% nth1_1(+N, +List, ?Element): Element is the Nth element of List
nth1_1(N, [H|T], E):-
	(   N = 1 -> E = H
	;   N > 1 -> N1 is N-1, nth1_1(N1, T, E)
	).

% delete_1(+L, +E, -R): R is L without any elements E
delete_1(L, E, R):-
	delete_1(L, E, [], R).

% delete_1(+L, +E, +R0, -R): R is L without any elements E appended to R0,
% in reverse order
delete_1([], _E, R, R).
delete_1(   [H|T], E, R0, R):-
	(   H = E -> R1 = R0
	;   R1 = [H|R0]
	),
	delete_1(T, E, R1, R).