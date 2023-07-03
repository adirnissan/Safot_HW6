:- [hw5_q2].
:- use_module(library(clpfd)).

perm([], []).
perm([X|L], P) :-
    perm(L, L1),
    insert(X, L1, P).

del(X, [X|Xs], Xs).
del(X, [Y|Ys], [Y|Zs]) :- del(X,Ys,Zs).

insert(X, L, R) :- del(X, R, L).

topological_sort(graph(G), [X|XS]) :- 
    legal_graph(graph(G)), dag(graph(G)),
    length(G, N), check_perm(N, [X|XS]),
    topological_sort_aux(graph(G), [X|XS]).
topological_sort_aux(graph(_), []).
topological_sort_aux(graph(G), [X|XS]) :-
    no_path(graph(G), X, XS),
    topological_sort_aux(graph(G), XS).

no_path(graph(_), _, []).
no_path(graph(G), X, [Y|YS]) :- 
    \+ path(graph(G), Y, X, _),
    no_path(graph(G), X, YS).

check_perm(N, X) :- list_up_to(N, L), perm(L, X).

list_up_to(1, [1]) :- !.
list_up_to(N, L) :- N1 is N-1, list_up_to(N1, L1), append(L1, [N], L).

scc(graph(G), S) :- 
    legal_graph(graph(G)),
    length(G,N),
    find_groups(graph(G), N, [], S).

/* find_groups returns the scc of the graph*/
find_groups(graph(_), 0, X, X) :- !.
find_groups(graph(G), I, X, R) :- 
    flatten(X, Xres), \+ member(I, Xres),
    length(G,N),
    find_vertex_group(graph(G), I, N, [], RofI),
    append([RofI], X, X1), I1 is I-1, find_groups(graph(G), I1, X1, R), !.
find_groups(graph(G), I, X, R) :- 
    I1 is I-1, find_groups(graph(G), I1, X, R).

/* find_vertex_function returns the scc of the vertex V*/
find_vertex_group(graph(_), _, 0, X, X) :- !.
find_vertex_group(graph(G), V, V, R, X) :-
    append([V],R,R1), I1 is V-1, find_vertex_group(graph(G), V, I1, R1, X), !.
find_vertex_group(graph(G), V, I, R, X) :-
    path(graph(G), V, I, _),
    path(graph(G), I, V, _),
    append([I],R,R1), I1 is I-1, find_vertex_group(graph(G), V, I1, R1, X), !.
find_vertex_group(graph(G), V, I, R, X) :- 
    I1 is I-1, find_vertex_group(graph(G), V, I1, R, X).

