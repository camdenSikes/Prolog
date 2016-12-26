% foai(+L, ?I): The first odd element of L is at index I (counted from 1)
foai(L, I):-
	foai(L, 0, I).

% foai(+L, +I, ?RI): The first odd elemfent of L is at index RI-I
foai([H|T], I, RI):-
	X is H rem 2,
	(1 is abs(X) -> RI is I+1
	;X = 0 -> I1 is I+1, foai(T, I1, RI)
	).

% pbfo(+L, ?F0, ?N): The number of positive elements before the fist odd
% element F0 is N
pbfo(L, F0, N):-
	pbfo(L, 0, F0, N).

% pbfo(+L, +N, ?F0, ?RN): The number of positive elements before the fist
% odd element F0 is RN - N
pbfo([H|T], N, F0, RN):-
	X is H rem 2,
	(1 is abs(X) -> F0 = H, RN = N
	;X = 0 ->
	    (H > 0 -> N1 is N+1
	    ;N1 = N
	    ),
	    pbfo(T, N1, F0, RN)
	).

% pbfoai(+L, ?FO, ?I, ?N): The number of positive elements before
% the first odd element FO, occurring at index I is N.
pbfoai(L, F0, I, N):-
	pbfoai(L, 0, 0, F0, I, N).

% pbfoai(+L, ,+I, +N, ?FO, ?RI, ?RN): The number of positive elements
%  before the first odd element FO, occurring at index RI-I is RN-N.
pbfoai([H|T], I, N, F0, RI, RN):-
	X is H rem 2,
	(1 is abs(X) -> F0 = H, RN = N, RI is I + 1
	;X = 0 ->
	    (H > 0 -> N1 is N+1
	    ;N1 = N
	    ),
	    I1 is I + 1,
	    pbfoai(T, I1, N1, F0, RI, RN)
	).

% fre(+L, ?E, ?I, ?Rest): Assume list L has a repeated element, i.e.
% an element with the right neighbour having the same value.
% The first repeated element in L is E, it occurs at index I and the
% list of remaining elements after the two neighbours is Rest.
fre(L, E, I, Rest):-
	fre(L, 0, E, I, Rest).

% fre(+L, +I, ?E, ?RI, ?Rest): Assume list L has a repeated element, i.e.
% an element with the right neighbour having the same value.
% The first repeated element in L is E, it occurs at index RI-I and the
% list of remaining elements after the two neighbours is Rest.
fre([H1|[H2|T]], I, E, RI, Rest):-
	(H1 = H2 -> E = H1, RI is I + 1, Rest = T
	;I1 is I + 1, fre([H2|T], I1, E, RI, Rest)
	).