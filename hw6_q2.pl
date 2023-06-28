:- [hw5_q2].
:- use_module(library(lists)).


topological_sort(graph(G), S) :- 
    legal_graph(graph(G)), dag(graph(G)),
    length(G, N),
    topological_sort_aux(G, N, [], S).

topological_sort_aux(G, N, S, S) :- repeat(N, [], G), !. 

topological_sort_aux(G, N, L, S) :- 
    nth1(X, G, []), \+ member(X, L), 
    remove_edges_to_pit(X, G, G1),
    topological_sort_aux(G1, N, [X|L], S).

remove_edges_to_pit(_, [], []).

remove_edges_to_pit(Pit, [V|VS], [V1|VS1]) :- 
    (member(Pit, V) -> del(Pit, V, V1)) ; V1 = V,
    remove_edges_to_pit(Pit, VS, VS1).

repeat(0, _, []).   

repeat(N, X, [X|XS]) :- N1 is N - 1, repeat(N1, X, XS).

del(X, [X|XS], XS).
del(X, [Y|YS], [Y|ZS]) :- del(X, YS, ZS).