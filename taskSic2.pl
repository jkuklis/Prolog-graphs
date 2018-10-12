:- use_module(library(lists)).

podzbior([], _).
podzbior([X|L1], L2) :-
    member(X, L2),
    podzbior(L1, L2).

niepustePrzeciecie([X|_], L2) :- member(X, L2).
niepustePrzeciecie([X|L1], L2) :- \+ member(X, L2), niepustePrzeciecie(L1, L2).

okKrawedzie([e],[e]).
okKrawedzie([e|L], [e|[V]]) :- member(V,L).
okKrawedzie([a|L1], [a|L2]) :- 
    sort(L1, L1s), 
    sort(L2, L2s), 
    L1s = L2s.

posortowanyWybor([],[]).
posortowanyWybor([[V|L1]|AE], [[V|L2]|G]) :- 
    okKrawedzie(L1,L2), 
    posortowanyWybor(AE, G).

jestWyborem2(AE,G) :- 
    sort(AE, AEs), 
    sort(G, Gs), 
    posortowanyWybor(AEs, Gs).

redukujRepr(L1,L2) :-
    sort(L1, L1s),
    L1 = L1s,
    L2 = L1s.

okKrawedzie2([e],[e]).
okKrawedzie2([e|L], [e|[V]]) :- member(V,L).
okKrawedzie2([a|L1], [a|L2]) :-
    permutation(L1, L2),
    redukujRepr(L1, L2).

znajdzSprawdz([V|L1],[[V|L2]|_]) :- okKrawedzie2(L1,L2).
znajdzSprawdz(N,[_|G]) :- znajdzSprawdz(N,G).

jestPWyborem([],_).
jestPWyborem([N|AE],G) :- 
    znajdzSprawdz(N,G), 
    jestPWyborem(AE,G).

redukujRepr2(G) :-
    sort(G, Gs),
    G = Gs.

jestWyborem(AE,G) :- 
    ground(AE), 
    ground(G), 
    jestWyborem2(AE,G).

jestWyborem(AE,G) :- 
    \+ ground(AE); \+ ground(G), 
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
    AE = [[V|_]|_],
    LR = [[V|_]|_],
    adfs(AE, LR, [], [], push),
    zwezona(LR, L).
