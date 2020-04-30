## CS 314 Final Project

The language is parsed using [Lark](https://github.com/lark-parser/lark).
Example input files can be found in [examples](/examples).

### Setup

You need to have python3 and use `pip3` to install Lark.
For example, `pip3 install lark-parser`.

### Implementation

Implement the functions for Problem 1-5 in src/final.py

### Examples and Test-cases

```
$ python3 src/main.py examples/starkhouse.pl
Print Input Program:
rule: father(rickard, ned) :- ()
rule: father(ned, robb) :- ()
rule: ancestor(X, Y) :- father(X, Y)
rule: ancestor(X, Y) :- (father(X, Z), ancestor(Z, Y))
?- ancestor(X, robb).
Print Input Goal:
Query: ancestor(X, robb)
```

```
$ python3 src/main.py examples/append.pl
Print Input Program:
rule: append(nil, Q, Q) :- ()
rule: append(cons(H, P), Q, cons(H, R)) :- append(P, Q, R)
?- append(X, Y, cons(1, cons(2, cons(3, nil)))).
Print Input Goal:
Query: append(X, Y, cons(1, cons(2, cons(3, nil))))
```
