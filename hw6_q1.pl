add(bot, R, R) :- !.
add(s(L), R, Sum) :- add(L, s(R), Sum).

multiply(bot, _, bot) :- !.
multiply(_, bot, bot) :- !.
multiply(L, R, Multi) :- multiply_aux(L ,R ,R ,Multi).

multiply_aux(s(bot), Multi, _ , Multi) :- !.
multiply_aux(s(L), R, OrigR, Multi) :- 
    add(R, OrigR, UpdatedR),
    multiply_aux(L, UpdatedR, OrigR, Multi).

power(_, bot, s(bot)) :- !.
power(Base, Exp, Pow) :- power_aux(Base, Exp, Base, Pow).

power_aux(Base, s(bot), _ , Base) :- !. 
power_aux(Base, s(Exp), OrigBase, Pow) :- 
    multiply(Base, OrigBase, UpdatedBase), 
    power_aux(UpdatedBase, Exp, OrigBase, Pow).