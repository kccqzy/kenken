#!/usr/bin/env swipl
%% -*- mode: prolog; -*-

%% NOTE This file is meant to be used with SWI-Prolog.
:- use_module(library(clpfd)).

length_flipped(N, L) :- length(L, N).

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
    maplist(all_distinct, G),
    transpose(G, GT),
    maplist(all_distinct, GT),
    maplist(label, G).

%% Test cases

test0(T) :-
    kenken(3, [], T).

test1(T) :-
    kenken(4,
           [-(1, [1|1], [1|2]),
            /(2, [1|3], [2|3]),
            *(12, [[4|1],[4|2],[4|3]]),
            +(9, [[3|1], [3|2], [4|2]]),
            +(6, [[3|3],[4|3],[4|4]]),
            /(2, [2|1], [2|2]),
            +(1, [[4|1]])
           ], T).

test2(T) :-
    kenken(
        6,
        [
            +(11, [[1|1], [2|1]]),
            /(2, [1|2], [1|3]),
            *(20, [[1|4], [2|4]]),
            *(6, [[1|5], [1|6], [2|6], [3|6]]),
            -(3, [2|2], [2|3]),
            /(3, [2|5], [3|5]),
            *(240, [[3|1], [3|2], [4|1], [4|2]]),
            *(6, [[3|3], [3|4]]),
            *(6, [[4|3], [5|3]]),
            +(7, [[4|4], [5|4], [5|5]]),
            *(30, [[4|5], [4|6]]),
            *(6, [[5|1], [5|2]]),
            +(9, [[5|6], [6|6]]),
            +(8, [[6|1], [6|2], [6|3]]),
            /(2, [6|4], [6|5])
        ], T
    ).

dotests :-
    findall(T0, test0(T0), T0s), maplist(portray_clause, T0s), nl,
    test1(T1), portray_clause(T1), nl,
    test2(T2), portray_clause(T2), nl.

:- initialization(dotests, main).
