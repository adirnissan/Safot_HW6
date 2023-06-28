add(bot, R, R) :- !.
add(d(L), R, Sum) :- add(L, d(R), Sum).

multiply(bot, _, bot) :- !.
multiply(_, bot, bot) :- !.
multiply(L, R, Multi) :- multiply_aux(L ,R ,R ,Multi).
multiply_aux(d(bot), Multi, _ , Multi) :- !.
multiply_aux(d(L), R, OrigR, Multi) :- add(R, OrigR, UpdatedR), multiply_aux(L, UpdatedR, OrigR, Multi).

power(_, bot, d(bot)) :- !.

power(Base, Exp, Pow) :- power_aux(Base, Exp, Base, Pow).

power_aux(Base, d(bot), _ , Base) :- !.

power_aux(Base, d(Exp), OrigBase, Pow) :- 
    multiply(Base, OrigBase, UpdatedBase), 
    power_aux(UpdatedBase, Exp, OrigBase, Pow).