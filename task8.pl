:- use_module(library(lists)).

nieElement(_, []).
nieElement(W, [V|L]) :-
    W \= V,
    nieElement(W, L).    

unikalne([]).
unikalne([W|L]) :-
    nieElement(W, L),
    unikalne(L).

podzbior([], _).
podzbior([X|L1], L2) :-
    podzbior(L1, L2),
    member(X, L2).

niepustePrzeciecie([X|_], L2) :- member(X, L2).
niepustePrzeciecie([X|L1], L2) :- \+ member(X, L2), niepustePrzeciecie(L1, L2).

listaWierzcholkow([], []).
listaWierzcholkow([[V|_]|AE], [V|L]) :-
    listaWierzcholkow(AE, L).

listaNumerow([0]).
listaNumerow([N|L]) :-
    listaNumerow(L),
    length(L,N).

poprawnyGraf([], L) :-
    listaNumerow(L).

poprawnyGraf([ [V,T|N] | L ], Vert) :-
    listaWierzcholkow(L, W),
    unikalne(N),
    append(Vert, [V|W], A),
    podzbior(N, A),
    poprawnyGraf(L, [V|Vert]),
    podzbior([T], [a,e]).

poprawnyGraf(G) :- var(G), poprawnyGraf(G, []).

poprawnyGraf(G) :- nonvar(G).

redukujRepr(L1,L2) :-
    sort(L1, L1s),
    L1 = L1s,
    L2 = L1s.

aeKrawedzie([e],[e]).
aeKrawedzie([e|L], [e|[V]]) :- member(V,L).
aeKrawedzie([a|L1], [a|L2]) :-
    permutation(L1, L2),
    redukujRepr(L1, L2).

znajdzSprawdz([V|L1],[[V|L2]|_]) :- aeKrawedzie(L1,L2).
znajdzSprawdz(N,[_|G]) :- znajdzSprawdz(N,G).

jestPWyborem([],_).
jestPWyborem([N|AE],G) :- 
    znajdzSprawdz(N,G),
    jestPWyborem(AE,G).

redukujRepr2(G) :-
    sort(G, Gs),
    G = Gs.

jestWyborem(AE,G) :-
    poprawnyGraf(AE),
    length(AE,X),
    length(G,X),
    jestPWyborem(AE,G),
    redukujRepr2(G).


dfs(_, [], [], [_|_]).

dfs(AE, [F|L], [], []) :- 
    member(F,AE),    
    F = [W|_],
    dfs(AE, L, [F], [W]).

dfs(AE, [], [T|S], P) :- 
    T = [_,_|N], 
    podzbior(N, P), 
    dfs(AE, [], S, P).

dfs(AE, [F|L], [T|S], P) :-
    member(F, AE),
    F = [W|_],
    \+ member(W, P),
    T = [_,_|N],
    member(W, N),
    dfs(AE, L, [F,T|S], [W|P]).

dfs(AE, [F|L], [T|S], P) :-
    member(F, AE),
    F = [W|_],
    \+ member(W, P),
    T = [_,_|N],
    \+ member(W, N),
    podzbior(N, P),
    dfs(AE, [F|L], S, P).

zwezona([], []).

zwezona([[V|_]|LR], [V|L]) :-
    zwezona(LR, L).

jestDFS([], []).

jestDFS(AE, L) :-
    poprawnyGraf(AE),
    AE = [[V|_]|_],
    LR = [[V|_]|_],
    dfs(AE, LR, [], []),
    zwezona(LR, L).

jestADFS(AE, G, L) :- jestWyborem(AE, G), jestDFS(G,L).

jestADFS(AE, L) :- jestADFS(AE, _, L).


adfs(_, [], [], _, pop).


adfs(AE, [F|L], [], [], _) :-
    member(F, AE),
    F = [W|_],
    adfs(AE, L, [F], [W], push).


adfs(AE, [], [T|S], P, _) :-
    T = [_,a|N],
    podzbior(N, P),
    adfs(AE, [], S, P, pop).

adfs(AE, [], [T|S], P, _) :-
    T = [_,e],
    adfs(AE, [], S, P, pop).

adfs(AE, [], [T|S], P, _) :-
    T = [_,e|N],
    niepustePrzeciecie(N, P),
    adfs(AE, [], S, P, pop).


adfs(AE, [F|L], [T|S], P, push) :-
    member(F, AE),
    F = [W|_],
    \+ member(W, P),
    T = [_,_|N],
    member(W, N),
    adfs(AE, L, [F,T|S], [W|P], push).


adfs(AE, [F|L], [T|S], P, push) :-
    member(F, AE),
    F = [W|_],
    \+ member(W, P),
    T = [_,a|N],
    \+ member(W, N),
    podzbior(N, P),
    adfs(AE, [F|L], S, P, pop).   

adfs(AE, [F|L], [T|S], P, push) :-
    member(F, AE),
    F = [W|_],
    \+ member(W, P),
    T = [_,e],
    adfs(AE, [F|L], S, P, pop).

adfs(AE, [F|L], [T|S], P, push) :-
    member(F, AE),
    F = [W|_],
    \+ member(W, P),
    T = [_,e|N],
    \+ member(W,N),
    niepustePrzeciecie(N, P),
    adfs(AE, [F|L], S, P, pop).


adfs(AE, [F|L], [T|S], P, pop) :-
    T = [_,e|_],
    adfs(AE, [F|L], S, P, pop).

adfs(AE, [F|L], [T|S], P, pop) :-
    T = [_,a|N],
    F = [W|_],
    member(W,N),
    adfs(AE, L, [F,T|S], [W|P], push).
       
adfs(AE, [F|L], [T|S], P, pop) :-
    T = [_,a|N],
    F = [W|_],
    \+ member(W,N),
    podzbior(N, P),
    adfs(AE, [F|L], S, P, pop).

jestADFS1([], []).

jestADFS1(AE, L) :-
    poprawnyGraf(AE),
    AE = [[V|_]|_],
    LR = [[V|_]|_],
    adfs(AE, LR, [], [], push),
    zwezona(LR, L).
