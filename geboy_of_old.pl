:- [hw5_q2].
:- use_module(library(clpfd)).


topological_sort(graph(G), S) :- 
    legal_graph(graph(G)), dag(graph(G)),
    topological_sort_aux(graph(G), [], S).

topological_sort_aux(graph(G), S, S) :- length(G, N), length(S, N), !.

topological_sort_aux(graph(G), SoFarS, S) :-
    find_source(graph(G), X), 
    append(SoFarS, [X], SoFarSButBigger),
    replace_nth1(G, X, [X] ,Gtag),
    topological_sort_aux(graph(Gtag), SoFarSButBigger, S).
 
 
replace_nth1([_|XS], 1 ,NewElem ,[NewElem|XS]) :- !.
replace_nth1([X|XS], N, NewElem ,[X|YS]) :- N1 is N-1, replace_nth1(XS, N1, NewElem, YS).

find_source(graph(G), X) :-
    length(G, N),
    is_vertex_source(graph(G), N, X).

is_vertex_source(graph(G), V, V) :- \+ our_edge(graph(G), _, V).

is_vertex_source(graph(G), V, X) :- V > 1 ,V1 is V-1, is_vertex_source(graph(G), V1, X).

our_edge(graph(G), X, Y) :- nth1(X, G, NeighborsX), member(Y, NeighborsX).

scc(graph(G), S):-
    legal_graph(graph(G)),
    check_strongly_connected_groups(graph(G), S),
    check_max_components(graph(G), S),
    check_valid_partition(graph(G), S).

/* check_strongly_connected_groups  */

check_strongly_connected_groups(graph(_), []).

check_strongly_connected_groups(graph(G), [C|CS]) :- 
    check_strongly_connected_group(graph(G), C),
    check_strongly_connected_groups(graph(G), CS).

check_strongly_connected_group(graph(_), []).

check_strongly_connected_group(graph(G), [V|VS]) :-
    check_vertex_connected_to_all(graph(G), V, VS),
    check_strongly_connected_group(graph(G), VS).

check_vertex_connected_to_all(graph(_), _, []).

check_vertex_connected_to_all(graph(G), V, [U|US]) :-
    path(graph(G), V, U, _),
    path(graph(G), U, V, _),
    check_vertex_connected_to_all(graph(G), V, US).

/* check_max_components */

check_max_components(graph(_), []).

check_max_components(graph(G), [C|CS]) :- 
    check_max_component(graph(G), C),
    check_max_components(graph(G), CS).

check_max_component(graph(G), C) :-
    length(G, N),
    check_cant_add_vertex(graph(G), N, C).

check_cant_add_vertex(graph(_), 0, _) :- !.

check_cant_add_vertex(graph(G), V, C) :-
    member(V, C),
    V1 is V-1,
    check_cant_add_vertex(graph(G), V1, C), !.

check_cant_add_vertex(graph(G), V, C) :-
    \+ check_vertex_connected_to_all(graph(G), V, C),
    V1 is V-1,
    check_cant_add_vertex(graph(G), V1, C).

/* check_valid_partition */

check_valid_partition(graph(G), P) :-
    length(G, N),
    check_vertex_in_partition(N, P).

check_vertex_in_partition(0, _) :- !. 

check_vertex_in_partition(V, P) :- 
    member(C, P),
    member(V, C),
    V1 is V-1,
    check_vertex_in_partition(V1, P).



    /****************************************************************************************/


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
    check_strongly_connected_groups(graph(G), S),
    check_max_components(graph(G), S),
    legal_partition(graph(G), S).
    
legal_partition(graph(G), S) :- 
    length(G, N),
    flatten(S, Sflatten), 
    check_perm(N, Sflatten).

/* check_strongly_connected_groups  */

check_strongly_connected_groups(graph(G), [C]) :- 
    check_strongly_connected_group(graph(G), C), !.

check_strongly_connected_groups(graph(G), [C|CS]) :- 
    check_strongly_connected_group(graph(G), C),
    check_strongly_connected_groups(graph(G), CS).

check_strongly_connected_group(graph(G), [V]) :- !.

check_strongly_connected_group(graph(G), [V|VS]) :-
    check_vertex_connected_to_all(graph(G), V, VS),
    check_strongly_connected_group(graph(G), VS).

check_vertex_connected_to_all(graph(_), _, []).

check_vertex_connected_to_all(graph(G), V, [U|US]) :-
    path(graph(G), V, U, _),
    path(graph(G), U, V, _),
    check_vertex_connected_to_all(graph(G), V, US).

/* check_max_components */

check_max_components(graph(G), [C]) :- 
    check_max_component(graph(G), C), !.

check_max_components(graph(G), [C|CS]) :- 
    check_max_component(graph(G), C),
    check_max_components(graph(G), CS).

check_max_component(graph(G), C) :-
    length(G, N),
    check_cant_add_vertex(graph(G), N, C).

check_cant_add_vertex(graph(_), 0, _) :- !.

check_cant_add_vertex(graph(G), V, C) :-
    member(V, C),
    V1 is V-1,
    check_cant_add_vertex(graph(G), V1, C), !.

check_cant_add_vertex(graph(G), V, C) :-
    \+ check_vertex_connected_to_all(graph(G), V, C),
    V1 is V-1,
    check_cant_add_vertex(graph(G), V1, C).



     

    
    