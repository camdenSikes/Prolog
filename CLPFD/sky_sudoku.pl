% Skyscraper sudoku
% By Camden Sikes

:- use_module(library(clpfd)).
:- use_module(library(lists)).

% skysudoku(SP, SS): SS is a solution of the Skyscraper Sudoku puzzle SP.
skysudoku(ss(Size,Clues), SP):-
	square(SP, Size),
	consistent(SP),
	apply_clues(SP, Clues),
	listify(SP,L),
	shave(L),
	labeling([ff], L).



%square(M, Size): M is a square matrix of side length Size^2).
square(M, Size):-
	Len is Size * Size,
	length(M,Len),
	square_helper(M, Len).

square_helper([H|T],Len):-
	length(H,Len),
	domain(H, 1, Len),
	square_helper(T,Len).
square_helper([],_L).

%apply_clues(SP, Clues): posts constraints on SP corresponding to the elements
%of Clues
apply_clues(_SP, []).
apply_clues(SP,[H|T]):-
	(H = g(A,B,C) -> given_value(SP, A, B, C)
	;H = v(A,B,C) -> visible_constraint(SP, A, B, C)
	;fail
	),
	apply_clues(SP,T).

given_value(SP,N,R,C):-
	nth1(R, SP, Row),
	nth1(C, Row, Elem),
	Elem #= N.

visible_constraint(SP,V,Dir,RC):-
	(Dir = w -> nth1(RC,SP,L)
	;Dir = e -> nth1(RC,SP,L1), reverse(L,L1)
	;Dir = n -> transpose(SP,SP1), nth1(RC,SP1,L)
	;Dir = s -> transpose(SP,SP1), nth1(RC,SP1,L1), reverse(L,L1)
	;fail),
	visnum(L,V).

% visnum(+L, ?K): the number of elements in list L that are visible from
% the left is K. It can be assumed that L is proper (i.e., not open-ended).
% The elements of L as well as K are FD-variables or integers.
visnum(L, K):-
	visnum(L, K, 0).
visnum([], 0, _Max).
visnum([H|T], K, Max):-
	(H #> Max) #<=> X,
	Max1 #= max(H,Max),
	K #= X + Y,
	visnum(T, Y, Max1).


%listify(+M, ?L): turns matrix M into list L.
listify([],[]).
listify([H|T],L):-
	listify(T,T1),
	append(H,T1,L).

%shave(L): shaves all the values in L
shave([]).
shave([H|T]):-
	shave_all(H),
	shave(T).

shave_all(X) :-
	fd_set(X, FD), fdset_to_list(FD, L),
	findall(X, member(X,L), Vs),
	list_to_fdset(Vs, FD1), X in_set FD1.






	











%------------------------------------------------------------
%------------------------------------------------------------

% consistent(+SGrid): For all areas of the Sudoku grid SGrid it holds
% that all positive integers in the area are distinct.
consistent(SGrid):-
	valid_rows(SGrid),
	valid_columns(SGrid),
	valid_boxes(SGrid).


% valid_rows(+M): constrains rows to contain no repeats
valid_rows([]).
valid_rows([H|T]):-
	all_distinct(H),
	valid_rows(T).

% valid_columns(+M): constrains columns to contain no repeats
valid_columns(M):-
	transpose(M,MT),
	valid_rows(MT).

% valid_boxes(+M): constrains boxes to contain no repeats
valid_boxes(M):-
	length(M, N),
	NB is integer(sqrt(N)),
	valid_boxes(M, NB, NB).

% valid_boxes(+M): constrains boxes to contain no repeats
valid_boxes(_M, _NR, 0).
valid_boxes([[]|T], NR, NB):-
	delete(T, [], R),
	NB1 is NB - 1,
	valid_boxes(R, NR, NB1).
valid_boxes([H|T], NR, NB):-
	H \= [],
	frstnmnz([H|T], NR, NR, L, M),
	all_distinct(L),
	valid_boxes(M, NR, NB).

% frstnmnz(+Matrix, +NR, +N, ?RL, -Rest): RL is all elements 
% of he first N elements of the first NR rows of Matrix.
% Rest is the matrix without the first N elements from the first NR rows
frstnmnz(M, NR, N, RL, R):-
	NE is NR*N,
	frstnmnz(M, NE, N, [], RL, [], R).

% frstnmnz(+Matrix, +NE, +N, +RL0, ?RL, +R0, -Rest): RL is an unordered
% list of elements from NE elements of Matrix taking N from each
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
	RL1 = [H|RL0],
	frstnmnz(M, NE1, N, RL1, RL, R1, R).