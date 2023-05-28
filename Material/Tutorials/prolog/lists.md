# Prolog

## lists

---

### Lists

In Prolog, a **list** is a sequence of any number of terms, separated by commas and enclosed in square brackets:

```prolog
[1, 2, 3]
[a, b, c, d]
[a, [1, 2, 3], tom, 15, date(1, may, 1995)]
```

<!--vert-->

In fact, a list is actually a pair of a head and a tail (which is a list itself):

```prolog
[Head | Tail]
[A, B, C] = [A | [B | [C | []]]] = [A, B | [C]]
```

> Kind of like SML lists.

---

## Built-in list predicates
<!--vert-->

### length/2

`length(L, N)` is satisfied when `L` is a list of length `N`.

```prolog
?- length([a, b, [c, d], e], N).

?- length(L, 4).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### length/2

`length/2` is implemented as follows:

```prolog
length([], 0).
length([_|Tail], N) :-
    length(Tail, N1),
    N is 1 + N1.
```

<!--vert-->

### is_list/1

`is_list(X)` is satisfied when `X` is a list.

```prolog
?- is_list(17).
?- is_list([1, 2, 3]).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

How would you implement this predicate?

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->


<!--vert-->

### is_list/1

```prolog
is_list([]).
is_list([X|Xs]) :- is_list(Xs).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### member/2

`member(X, L)` is satisfied when `X` is a member of `L`.

```prolog
?- member(X, [17, 13, 2, 5]).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

How would you implement this predicate?

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### member/2

```prolog
member(X, [X|Xs]).
member(X, [Y|Ys]) :- member(X, Ys).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### prefix/2

`prefix(X, L)` is satisfied when `X` is a prefix of `L`.

```prolog
?- prefix(X, [a, b, c, d]).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

How would you implement this predicate?

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

### prefix/2

```prolog
prefix([], L).
prefix([X|Xs], [X|Ys]) :- prefix(Xs, Ys).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### suffix/2

`suffix(X, L)` is satisfied when `X` is a suffix of `L`.

```prolog
?- suffix(X, [1, 2, 3]).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

How would you implement this predicate?

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### suffix/2

```prolog
suffix(Xs, Xs).
suffix(Xs, [Y|Ys]) :- suffix(Xs, Ys).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

---

## List Exercises

Implement the following (non built-in) predicates.

<!--vert-->

#### del/3

`del(X, L, R)` is satisfied when `R` is `L` without one of the occurrences of `X`.

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

```prolog
?- del(2, [1, 2, 3, 2, 3, 2], X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### del/3

```prolog
del(X, [X|Xs], Xs).
del(X, [Y|Ys], [Y|Zs]) :- del(X, Ys, Zs).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

```prolog
?- del(2, [1, 2, 3, 2, 3, 2], X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### insert/3

`Insert(X, L, R)` is satisfied when `R` is `L` with an additional occurrence of `X`.

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

```prolog
?- insert(3, [1, 2, 3], X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### insert/3

```prolog
insert(X, L, R) :- del(X, R, L).
```
```prolog
?- insert(3, [1, 2, 3], X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### append/3

`append(X, Y, Z)` is satisfied when `Z` is the concatenation of `X` and `Y` (in that order).

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

```prolog
?- append([1, 2], [3, 4, 5], X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### append/3

```prolog
append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

```prolog
?- append([1, 2], [3, 4, 5], X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### define member/2 using append/3

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
member(X, Xs) :- append(_, [X|_], Xs).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### sublist/2

`sublist(X, Y)` is satisfied when `X` is a sublist of `Y`.

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### sublist/2

```prolog
sublist(Xs, Ys) :-
    append(As, Bs, Ys),
    append(Xs, Cs, Bs).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

```prolog
prefix(Xs, Ys) :- append(Xs, _, Ys).

suffix(Xs, Ys) :- append(_, Xs, Ys).

sublist(Xs, Ys) :-
    prefix(Ps, Ys),
    suffix(Xs, Ps).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### permutation/2

`permutation(X, Y)` is satisfied when `X` is a permutation of `Y`.

```prolog
...
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

```prolog
?- permutation([1, 2, 3], X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

<!--vert-->

#### permutation/2

```prolog
permutation([], []).
permutation([X|L], P) :-
    permutation(L, L1),
    insert(X, L1, P).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->

```prolog
?- permutation([1, 2, 3], X).
```
<!-- .element: data-thebe-executable-prolog data-language="text/x-prolog" -->
