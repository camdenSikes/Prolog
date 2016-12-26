/*Camden Sikes
Practice 2 */

% tree_depth(+Tree, ?D): Tree is a binary tree of depth D.
tree_depth(leaf(_), 0).
tree_depth(node(L,R), D):-
	tree_depth(L,D1),
	tree_depth(R,D2),
	D is max(D1,D2)+1.

% tree_leaf_value(+Tree, +V): V is present as a leaf value in Tree.
tree_leaf_value(leaf(X), X).
tree_leaf_value(node(L,_R), V):-
	tree_leaf_value(L, V).
tree_leaf_value(node(_L,R), V):-
	tree_leaf_value(R, V).

% increment_tree(+Tree0, ?Tree): Tree is obtained from binary tree
% Tree0 by incrementing each leaf value by 1.
increment_tree(leaf(X), leaf(Y)):-
	Y is X+1.
increment_tree(node(L,R),node(L1,R1)):-
	increment_tree(L,L1),
	increment_tree(R,R1).

% tree_rightmost_value(+Tree, ?Value): Tree is a binary tree and
% the integer in its rightmost leaf is Value.
tree_rightmost_value(leaf(V),V).
tree_rightmost_value(node(_L,R),V):-
	tree_rightmost_value(R,V).

% subtree(+Tree, -SubTree): SubTree is a subtree of Tree.
subtree(leaf(X),leaf(X)).
subtree(node(L,R),node(L,R)).
subtree(node(L,_R),S):-
	subtree(L,S).
subtree(node(_L,R),S):-
	subtree(R,S).

% list_length(+List, -Len): List is a list whose length is Len.
list_length([],0).
list_length([_H|T], L):-
	list_length(T,L1),
	L is L1+1.

% list_element(+List, +V): V is present as an element in the list List.
list_element([H|_T], X):-
	H = X.
list_element([_H|T], X):-
	list_element(T, X).

% increment_list(+L0, ?L): L is a list of numbers obtained
% from L0 by incrementing each element by 1.
% You can assume that L0 is given and is a list of numbers.
increment_list([], []).
increment_list([H|T], L):-
	H1 is H+1,
	increment_list(T,T1),
	L = [H1|T1].

% list_last(+List, ?V): List is a list whose last element is V.
list_last([H|[]],H).
list_last([_H|T],V):-
	list_last(T,V).

% list_suffix(+L0, -L): L is a suffix of L0.
list_suffix(L, L).
list_suffix([_H|T], L):-
	list_suffix(T, L).

% list_concat(+L1, +L2, -L3): L3 is the concatenation of lists L1 and L2.
list_concat([],L,L).
list_concat([H|T],L0,L):-
	list_concat(T,L0,L1),
	L = [H|L1].