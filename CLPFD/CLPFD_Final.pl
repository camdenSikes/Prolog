
% endviews
% by Camden Sikes

:- use_module(library(clpfd)).
:- use_module(library(lists)).

% endviews(+N, +M, ?List, +LV, +RV):
% The list List is of length N, and contains all numbers between 1 and M exactly once, and
% (N-M) instances of the number 0. LV (left view) is the first nonzero element in the list
% List and RV (right view) is the last nonzero element. All arguments, except List, are given
% integers, where N>=M, 1 =< LV =< M and 1 =< RV =< M also holds. Furthermore, LV \= RV
% holds unless M = 1. List is a variable or a proper list of FD variables/integers.
endviews(N, M, List, LV, RV):-
        length(List, N),
        domain(List, 0, M),
        Z is N - M,
        gen_card_list(M, Z, GCL),
        global_cardinality(List, GCL),
        firstpos(List, LV),
        reverse(List, Revlist),
        firstpos(Revlist, RV),
        shave(List).

% gen_card_list(+M, +N, ?GCL): L is the list needed to call global_cardinality when there is 1 each of 1 through M and Z zeros
gen_card_list(M, Z, GCL):-
        (M = 0 -> GCL = [0-Z]
        ;       M1 is M - 1,
                GCL = [M-1|GCL1],
                gen_card_list(M1, Z, GCL1)
        ).        
         
% firstpos(+L, +K): in list L, the first positive number is K. L is proper,
% The elements of L are FD-variables or integers; all elements of L are
% non-negative. K is a fixed positive integer. firstpos should  prune as
% much as possible, but not perform labeling nor create any choice points.
firstpos(L,K):-
        domain(L,0,sup),
        firstpos(L,K,0).
firstpos([],_,1).
firstpos([H|T],K,B):-
        (B #\/ (H #= K)) #<=> X,
        #\X #=> H #= 0,
        firstpos(T,K,X).
        

%shave(L): shaves all the values in L starting
shave([]).
shave([H|T]):-
        shave_all(H),
        shave(T).

shave_all(X) :-
        fd_set(X, FD), fdset_to_list(FD, L),
        findall(X, member(X,L), Vs),
        list_to_fdset(Vs, FD1), X in_set FD1.