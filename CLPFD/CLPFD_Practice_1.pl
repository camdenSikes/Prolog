% clpfd practice 1
% By Camden Sikes

:- use_module(library(clpfd)).
:- use_module(library(lists)).

% q(=A, ?B, ?C, ?D): A, B, C, D are positive integers, A=B+C+D, 2*B<C, 2*C<D
q(A, B, C, D):-
	domain([A, B, C, D], 1, sup),
	A #= B + C + D,
	C #> 2 * B,
	D #> 2 * C.


% sudoku_simple(?Matrix, +N): Matrix is an NxN matrix, containing integers
% between 1 and N. In each row and in each column, the numbers are pairwise
%  different.
sudoku_simple(Mx, N):-
	length(Mx, N),
	sudoku_simple_helper(Mx, N),
	transpose(Mx, MxT),
	length(MxT, N),
	sudoku_simple_helper(MxT, N).
sudoku_simple_helper([H|T], N):-
	length(H,N),
	domain(H,1,N),
	all_different(H),
	sudoku_simple_helper(T, N).
sudoku_simple_helper([], _).


% fibonacci(+N, +Limit, ?L): N and Limit are positive integers; L is a list 
% of length N, all elements of L are integers from the interval [1,Limit], 
% and for any three consecutive elements A, B, C of L, the equation C=A+B
% holds.
fibonacci(N, Limit, L):-
	length(L, N),
	domain(L, 1, Limit),
	fibonacci_helper(L, Limit).
fibonacci_helper([H|T], Limit):-
	(length(T,N), N > 1 ->
	    T = [A,B|_C],
	    B #= H+A,
	    fibonacci_helper(T, Limit)
	; true
	).


% p(?A, ?B): if dividing A by 3 gives remainder 1, then B is even,
% otherwise B is odd.
p(A, B):-
	X #= A mod 3,
	Y #= B + X,
	1 #= Y mod 2.

% knapsack(+Items, +MaxWeight, +MinValue, ?Selected): Items is a list of 
% items of the form item(Id,Weight,Value), where Id, Weight, and Value are 
% given positive integers; Id is a unique identifier of the item, whereas 
% Weight and Value specify its weight and value. MaxWeight and MinValue are
% given positive integers specifying the capacity of the knapsack and the
% minimum total value of the items to be selected, respectively. Selected 
% should be a list of the identifiers of the selected items, in the same 
% order as they appear in Items. knapsack/4 should include labeling.
knapsack(Items, MaxW, MinV, Selected):-
	id_weight_value(Items, IDL, WL, VL),
	length(Items, N),
	length(Vars, N),
	domain(Vars, 0, 1),
	scalar_product(WL, Vars, #=<, MaxW),
	scalar_product(VL, Vars, #>=, MinV),
	labeling([], Vars),
	create_output(IDL, Vars, Selected).
	


% weight_and_value(+Items, ?IDL, ?WL, ?VL): Items is a list of items of the 
% form item(Id,Weight,Value), IDL, WL and VL are lists of the ids, weights
%  and values in order
% id_weight_value(Items, IDL, WL, VL):-
% 	id_weight_value(Items, [], [], [], IDL, WL, VL).
id_weight_value([item(ID,W,V)|T], IDLA, WLA, VLA, IDL, WL, VL):-
	IDLA1 = [ID|IDLA],
	WLA1 = [W|WLA],
	VLA1 = [V|VLA],
	id_weight_value(T,IDLA1,WLA1,VLA1,IDL,WL,VL).
id_weight_value([],IDLA,WLA,VLA,IDL,WL,VL):-
	reverse(IDLA, IDL),
	reverse(WLA, WL),
	reverse(VLA, VL).

id_weight_value([],[],[],[]).
id_weight_value([item(ID,W,V)|T],[ID|IDL],[W|WL],[V|VL]):-
	       id_weight_value(T,IDL,WL,VL).


		
create_output([], [], []).
create_output([X|L1], [1|Vars], [X|L2]) :-
	create_output(L1, Vars, L2).
create_output([_|L1], [0|Vars], L2) :-
	create_output(L1, Vars, L2).