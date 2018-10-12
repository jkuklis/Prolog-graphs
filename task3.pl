podzbior([], _).
podzbior([V|N], K) :- member(V,K), podzbior(N,K).

okKrawedzie([e],[e]).
okKrawedzie([e|L], [e|[V]]) :- member(V,L).
okKrawedzie([a|L1], [a|L2]) :- msort(L1, L1s), msort(L2, L2s), L1s = L2s.

posortowanyWybor([],[]).
posortowanyWybor([[V|L1]|AE], [[V|L2]|G]) :- okKrawedzie(L1,L2), posortowanyWybor(AE, G).

jestWyborem2(AE,G) :- word("A"), msort(AE, AEs), msort(G, Gs), posortowanyWybor(AEs, Gs).

okKrawedzie2([e],[e]).
okKrawedzie2([e|L], [e|[V]]) :- member(V,L).
okKrawedzie2([a|L1], [a|L2]) :- length(L1, X), length(L2,X), podzbior(L1, L2), podzbior(L2, L1), msort(L1, L1s), L1 = L1s, msort(L2, L2s), L2 = L2s.

znajdzSprawdz([V|L1],[[V|L2]|_]) :- okKrawedzie2(L1,L2).
znajdzSprawdz(AE,[_|G]) :- znajdzSprawdz(AE,G).

jestPWyborem([],_).
jestPWyborem([V0|AE],G) :- znajdzSprawdz(V0,G), jestPWyborem(AE,G).

jestWyborem(AE,G) :- ground(AE), ground(G), jestWyborem2(AE,G).
jestWyborem(AE,G) :- 
    \+ ground(AE); \+ ground(G), 
    length(AE,X), 
    length(G,X), 
    jestPWyborem(AE,G),
    msort(G,Gs),
    G = Gs.

ktorysZ([[V|N]|_], V, N).
ktorysZ([_|AE], V, N) :- ktorysZ(AE, V, N).

sasiad([[V,_|N]|_], V, V1) :- member(V1, N).
sasiad([_|AE], V, V1) :- sasiad(AE, V, V1).

zejscia(AE, [V], K) :- 
    ktorysZ(AE, V, [_|N]),
    podzbior(N, K).

zejscia(AE, [V|L], K) :- 
    append([V1|L1], L2, L),
    sasiad(AE, V, V1), 
    zejscia(AE, [V1|L1], [V|K]),
    \+ member(V1, K),
    append([V1|L1], K, Kext),
    zejscia(AE, [V|L2], Kext).


jestDFS(AE, L) :- 
    length(AE,X), 
    length(L,X), 
    AE = [[V|_]|_], 
    L = [V|_], 
    zejscia(AE, L, []).


jestADFS(AE, G, L) :- jestWyborem(AE, G), jestDFS(G, L).

zejscia1(AE, [V], _, _) :-
    ktorysZ(AE, V, [e]).

zejscia1(AE, [V], _, X) :-
    ktorysZ(AE, V, [e|_]),
    X is 1.

zejscia1(AE, [V], _, X) :- 
    ktorysZ(AE, V, [a|N]),
    length(N,Y),
    X is Y.

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
    length(L,X), 
    AE = [[V|_]|_],
    L = [V|_], 
    zejscia1(AE, L, [], 0).
