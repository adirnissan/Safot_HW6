
legal_graph(graph([X|XS])) :- length([X|XS], MaxVertex), legal_list([X|XS], MaxVertex).

legal_list([], _).

legal_list([V|VS], MaxVertex) :- legal_vertex(V, MaxVertex), legal_list(VS, MaxVertex).

legal_vertex([], _).

legal_vertex([N|NS], MaxVertex) :- number(N), N >= 1, N =< MaxVertex, legal_vertex(NS, MaxVertex).


edge(graph(G), X, Y) :- legal_graph(graph(G)), nth1(X, G, NeighborsX), member(Y, NeighborsX).


path(graph(G), X, Y, P) :- legal_graph(graph(G)), dfs(graph(G), X, Y, P, []).

dfs(graph(G), X, Y, P, Forbidden) :- edge(graph(G), X, Y), \+ member(Y, Forbidden), P = [X, Y]. 

dfs(graph(G), X, Y, P, Forbidden) :- 
    nth1(X, G, NeighborsX), 
    dfs_neighbors(graph(G), X, Y, P, Forbidden, NeighborsX).

dfs_neighbors(graph(G), X, Y, P, Forbidden, [N|_]) :-
    \+ member(N, Forbidden),
    dfs(graph(G), N, Y, Ptmp, [X|Forbidden]),
    P = [X|Ptmp].

dfs_neighbors(graph(G), X, Y, P, Forbidden, [_|Ns]) :-
    dfs_neighbors(graph(G), X, Y, P, Forbidden, Ns).


circle(graph(G), P) :- path(graph(G), X, Y, P1), edge(graph(G), Y, X), append(P1, [X], P).


dag(graph(G)) :- \+ circle(graph(G), _).
