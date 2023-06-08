# Prolog

## CLP(FD)

---

### intro

`CLP(FD)` is a library that help us use constraints over integer values.
* CLP = **C**onstraint **L**ogic **P**rogramming.
* FD = **F**inite **D**omains.

<!--vert-->

### usage

in order to use `CLP(FD)`, write the following in the REPL:

```prolog
use_module(library(clpfd)).
```

...or write the following at the top of a file:

```prolog
:- use_module(library(clpfd)).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

---

### comparison operators

The comparison operators are almost the same but prefixed by `#`

```prolog
X #> Y.
X #< Y.
X #>= Y.
X #=< Y.
X #= Y.
X #\= Y.
```

<!--vert-->

### comparison operators

`X` and `Y` can be any arithmetic expression:

* An integer value.
* A variable.
* `-Expr`
* `Expr1 @ Expr2` where `@` is replaced by `+` `*` `-` `^` `//` `div` `mod` `rem`
* `abs(Expr)`
* `min(Expr1,Expr2)`
* `max(Expr1,Expr2)`

<!--vert-->

### comparison operators

How are these different from the regular comparison operators?

```prolog
?- X + 2 =:= Y + X.

?- X + 2 #= Y + X.
% Y = 2, X in inf..sup.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

The `#`-operators don't require that any of the variables are instantiated

---

### domains

`CLP(FD)` can give a domain as a solution:

```prolog
?- 0 #< X, X #< 5.
% X in 1..4.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### domains

In a domain, `sup` is for supremum and `inf` is for infimum.

```prolog
?- 0 #< X.
% X in 1..sup.

?- X #< 5.
% X in inf..4.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### domains

We can use the `in` operator in our code.

```prolog
?- X in 1..5.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### domains

`\/` is used for domains union

```prolog
?- X in 1..5, X #\= 2.
% X in 1\/3..5.

?- X in 1\/2\/3.
% X in 1..3.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### labeling

`indomain(X)` is used to successively bind `X` to all integers of its domain:

```prolog
?- X in 1..3, indomain(X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### labeling

`indomain` must always terminate:

```prolog
?- X in 0..sup, indomain(X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### labeling

`label` is just like `indomain` but for a list of variables:

```prolog
?- 0 #=< N, N #< 17, 0 #< A, 0 #< B, N * N #= A * A + B * B, label([N, A, B]).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

---

### question

Implement the predicate `change/2`. `change(S, L)` is true iff:

* `S` is a positive integer
* `L` is a **sorted** (descending) list made of the integers `1`, `5`, `10`
* `S` is the sum of the numbers in L

(assume `S` is concrete)

```prolog
?- change(21, [10, 5, 1, 1, 1, 1, 1, 1]).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

> Hint - you can use a helper predicate `repeat/3`. `repeat(N, C, L)` is true iff:
> * `N` is a conrete non-negative integer.
> * `L` is a list of `N` `C`s.

<!--vert-->

> Another Hint - you can use a helper predicate `build/2`. `build(Ls, L)` is true iff:
> * `Ls` is a list of pairs `[N, C]` where `N` is a concrete non-negative integer.
> * `L` is a list containing `Ni` `Ci`s, for each `i` in range.

<!--vert-->

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
repeat(0, _, []).
repeat(N, C, [C|Xs]) :-
	N #> 0,
	Ns #= N - 1,
	repeat(Ns, C, Xs).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->


```prolog
build([], []).
build([[N,C]|T], L) :- repeat(N, C, Xs), build(T, Ys), append(Xs, Ys, L).

change(S, L) :-
    S #= A1 + A5 * 5 + A10 * 10,
    A1 #>= 0, A5 #>= 0, A10 #>= 0,
    label([A1, A5, A10]),
    build([[A10, 10], [A5, 5], [A1, 1]], L)
.
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->
