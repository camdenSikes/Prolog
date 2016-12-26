%By Camden Sikes
:- use_module(library(clpfd)).
:- use_module(library(lists)).

% optnum(+L, ?K): the number of local optima in list L is K and the elements
% of L are pairwise non-equal. It can be assumed that L is proper (i.e., not
% open-ended). The elements of L as well as K are FD-variables or integers.
optnum(L,K):-
	all_different(L),
	optnumhelper(L,K).
optnumhelper([_A,_B],0).
optnumhelper([A,B,C|T], K):-
	((B #> A) #/\ (B #> C)) #\/ ((B #< A) #/\ (B #< C)) #<=> X,
	optnumhelper([B,C|T],Y),
	K #= X+Y.

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
	

% firstpos(+L, +K): in list L, the first positive number is K. L is proper,
% The elements of L are FD-variables or integers; all elements of L are
% non-negative. K is a fixed positive integer. firstpos should  prune as
% much as possible, but not perform labeling nor create any choice points.
firstpos(L,K):-
	domain(L,0,sup),
	firstpos(L,K,0).
firstpos([],_,1).
firstpos([H|T],K,B):-
	(B #\/ H #= K) #<=> X,
	#\X #=> H #= 0,
	firstpos(T,K,X).