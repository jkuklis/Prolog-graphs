okKrawedzie([e],[e]).
okKrawedzie([e|L], [e|[V]]) :- member(V,L).
okKrawedzie([a|L1], [a|L2]) :- 
    msort(L1, L1s), 
    msort(L2, L2s), 
    L1s = L2s.

posortowanyWybor([],[]).
posortowanyWybor([[V|L1]|AE], [[V|L2]|G]) :- 
    okKrawedzie(L1,L2), 
    posortowanyWybor(AE, G).

jestWyborem2(AE,G) :- 
    msort(AE, AEs), 
    msort(G, Gs), 
    posortowanyWybor(AEs, Gs).

redukujRepr(L1,L2) :-
    msort(L1, L1s),
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
    msort(G, Gs),
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

sasiad([[V,_|N]|_], V, V1) :- member(V1, N).
sasiad([_|AE], V, V1) :- sasiad(AE, V, V1).

zejscia(AE, [V], K) :- 
    member([V,_|N], AE),
    subset(N, K).

zejscia(AE, [V|L], K) :- 
    append([V1|L1], L2, L),
    sasiad(AE, V, V1), 
    zejscia(AE, [V1|L1], [V|K]),
    \+ member(V1, K),
    append([V1|L1], K, Kext),
    zejscia(AE, [V|L2], Kext).


osiagalne(_, [], _, []).

osiagalne(AE, [V|L], P, K) :-
    member(V, P),
    osiagalne(AE,L,P,K).

osiagalne(AE, [V|L], P, K) :-
    \+ member(V,P),
    member([V,_|N], AE),
    osiagalne(AE, N, [V|P], K1),
    union([V|P], K1, K2),    
    osiagalne(AE, L, K2, K3),
    union(K2, K3, K).


jestDFS([], []).
jestDFS(AE, L) :- 
    AE = [[V|_]|_],
    L = [V|_], 
    osiagalne(AE, [V], [], K),
    length(K,X), 
    length(L,X),
    zejscia(AE, L, []).

jestADFS(AE, L) :- jestWyborem(AE, G), jestDFS(G,L).

jestADFS(AE, G, L) :- jestWyborem(AE, G), jestDFS(G, L).

zejscia1(AE, [V], _, _) :-
    member([V,e], AE).

zejscia1(AE, [V], _, 1) :-
    member([V,e|_], AE).

zejscia1(AE, [V], _, X) :- 
    member([V,a|N], AE),
    length(N,X).

zejscia1(AE, [V|L], K, X) :- 
    append([V1|L1], L2, L),
    sasiad(AE, V, V1), 
    zejscia1(AE, [V1|L1], [V|K], 0),
    \+ member(V1, K),
    append([V1|L1], K, Kext),
    Y is X + 1,
    zejscia1(AE, [V|L2], Kext, Y).


jestADFS1(AE, L) :- 
    length(AE,X), 
    length(L,Y),
    Y =< X, 
    AE = [[V|_]|_],
    L = [V|_], 
    zejscia1(AE, L, [], 0).
