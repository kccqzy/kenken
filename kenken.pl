%% -*- mode: prolog; -*-

length_flipped(N, L) :- length(L, N).

numbers_up_to(0, []).
numbers_up_to(N, [N|R]) :-
    M + 1 #= N, numbers_up_to(M, R).

transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
    lists_firsts_rests(Ms, Ts, Ms1),
    transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss).

access([I|J], G, N) :-
    nth1(I, G, Row),
    nth1(J, Row, N).

apply_constraint(_, +(0, [])).
apply_constraint(G, +(S, [ Pos |Ls])) :-
    access(Pos, G, N),
    S #= N + M,
    apply_constraint(G, +(M, Ls)).
apply_constraint(_, *(1, [])).
apply_constraint(G, *(P, [ Pos |Ls])) :-
    access(Pos, G, N),
    P #= N * Q,
    apply_constraint(G, *(Q, Ls)).
apply_constraint(G, -(D, J, K)) :-
    access(J, G, N),
    access(K, G, M),
    ( D + M #= N; D + N #= M).
apply_constraint(G, /(Q, J, K)) :-
    access(J, G, N),
    access(K, G, M),
    ( Q * M #= N; Q * N #= M).

apply_all_constraints(C, G) :-
    maplist(apply_constraint(G), C).

kenken(N, C, G) :-
    length(G, N),
    maplist(length_flipped(N), G),
    maplist(maplist(#>=(N)), G),
    maplist(maplist(#=<(1)), G),
    apply_all_constraints(C, G),
    numbers_up_to(N, AllNumbers),
    maplist(permutation(AllNumbers), G),
    transpose(G, GT),
    maplist(permutation(AllNumbers), GT).
