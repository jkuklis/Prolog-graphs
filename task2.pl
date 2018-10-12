podzbior([], _).
podzbior([V|N], K) :- member(V,K), podzbior(N,K).

okKrawedzie([e],[e]).
okKrawedzie([e|L], [e|[V]]) :- member(V,L).
okKrawedzie([a|L1], [a|L2]) :- msort(L1, L1s), msort(L2, L2s), L1s = L2s.

posortowanyWybor([],[]).
posortowanyWybor([[V|L1]|AE], [[V|L2]|G]) :- okKrawedzie(L1,L2), posortowanyWybor(AE, G).

jestWyborem2(AE,G) :- msort(AE, AEs), msort(G, Gs), posortowanyWybor(AEs, Gs).

okKrawedzie2([e],[e]).
okKrawedzie2([e|L], [e|[V]]) :- member(V,L).
okKrawedzie2([a|L1], [a|L2]) :- length(L1, X), length(L2,X), podzbior(L1, L2), podzbior(L2, L1).

znajdzSprawdz([V|L1],[[V|L2]|_]) :- okKrawedzie2(L1,L2).
znajdzSprawdz(AE,[_|G]) :- znajdzSprawdz(AE,G).

jestPWyborem([],_).
jestPWyborem([V0|AE],G) :- znajdzSprawdz(V0,G), jestPWyborem(AE,G).

jestWyborem(AE,G) :- ground(AE), ground(G), jestWyborem2(AE,G).
jestWyborem(AE,G) :- 
    \+ ground(AE); \+ ground(G), 
    length(AE,X), 
    length(G,X), 
    jestPWyborem(AE,G).

ktorysZ([[V,_|N]|_], V, N).
ktorysZ([_|AE], V, N) :- ktorysZ(AE, V, N).

sasiad([[V,_|N]|_], V, V1) :- member(V1, N).
sasiad([_|AE], V, V1) :- sasiad(AE, V, V1).

zejscia(AE, [V], K) :- 
    ktorysZ(AE, V, N),
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


jestADFS(AE, L) :- jestWyborem(AE, G), jestDFS(G, L).
