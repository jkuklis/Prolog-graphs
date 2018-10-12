% Jakub Kuklis jk371125
% Zaimplementowane wszystkie funkcje, dzialaja dla pierwszego parametru danego 
% i dla obu nieznanych.
% Uznajemy za poprawne tylko grafy posortowane po nazwach wierzcholkow, 
% z posortowanymi listami sasiedztwa 
% (zeby to usunac, nalezy usunac predykaty o redukcji).

:- use_module(library(lists)).

% podzbiorUp(Podzbior, Zbior) - Podzbior jest uporzadkowanym podzbiorem Zbioru,
% uporzadkowanie takie jak w Zbiorze

podzbiorUp([], []).

podzbiorUp([X|P], [X|Z]):-
  podzbiorUp(P, Z).

podzbiorUp(P, [_|Z]):-
  podzbiorUp(P, Z).
 

% podzbior(Podzbior, Zbior) - Podzbior jest podzbiorem Zbioru

podzbior([], _).

podzbior([X|L1], L2) :-
    podzbior(L1, L2),
    member(X, L2).


% niepustePrzeciecie(L1, L2) - listy L1 i L2 maja niepuste przeciecie

niepustePrzeciecie([X|_], L2) :- member(X, L2).

niepustePrzeciecie([X|L1], L2) :- \+ member(X, L2), niepustePrzeciecie(L1, L2).


% listaWierzcholkow(AE, L) - L jest lista uporzadkowana nazw wierzcholkow grafu AE

listaWierzcholkow([], []).

listaWierzcholkow([[V|_]|AE], [V|L]) :-
    listaWierzcholkow(AE, L).


% listaNumerow(L) - L jest lista postaci [n, n-1, .., 0] dla n >= 0

listaNumerow([0]).

listaNumerow([N|L]) :-
    listaNumerow(L),
    length(L,N).


% poprawnyGraf(G, L) - G jest poprawnym grafem z wierzcholkami kolejno 
% o numerach od 0 do n, przy czym L jest lista przejrzanych juz wierzcholkow

poprawnyGraf([], L) :-
    listaNumerow(L).

% musimy upewnic sie m.in., ze lista sasiadow wierzcholka 
% zawiera sie w liscie wszystkich wierzcholkow grafu

poprawnyGraf([ [V,T|N] | L ], Vert) :-
    poprawnyGraf(L, [V|Vert]),
    listaWierzcholkow(L, W),
    append(W, Vert, A),   
    sort([V|A],As),
    podzbiorUp(N, As),
    podzbior([T], [a,e]).


% poprawnyGraf(G) - G nie jest zmienna, wtedy uznajemy, ze jest poprawny 
% (jezeli jest w calosci dany od uzytkownika, to zalozenie w zadaniu o poprawnosci wejscia)
% jezeli G jest zmienna, to konstruujemy kolejne poprawne grafy

poprawnyGraf(G) :- var(G), poprawnyGraf(G, []).

poprawnyGraf(G) :- nonvar(G).


% redukujRepr(L2) - upewniamy sie, ze lista jest posortowana

redukujRepr(L2) :-
    sort(L2, L2s),
    L2 = L2s.


% aeKrawedzie(L1, L2) - upewniamy sie, ze warunki AE-wyboru są spełnione 
% dla L1 z AE-grafu i L2 z AE-wyboru

aeKrawedzie([e],[e]).
aeKrawedzie([e|L], [e|[V]]) :- member(V,L).
aeKrawedzie([a|L1], [a|L2]) :-
    permutation(L1, L2),
    redukujRepr(L2).


% znajdzSprawdz(AE_L, G_L) - sprawdza, czy wierzcholek z AE-grafu
% pojawia się w AE-wyborze i jest spelnia wymogi AE-wyboru

znajdzSprawdz([V|L1],[[V|L2]|_]) :- aeKrawedzie(L1,L2).
znajdzSprawdz(N,[_|G]) :- znajdzSprawdz(N,G).


% jestPWyborem(AE_L, G_L) - sprawdza dla kazdego wierzcholka jego pojawienie sie
% w AE-wyborze G_L oraz poprawnosc tego wystapienia.

jestPWyborem([],_).
jestPWyborem([N|AE],G) :- 
    znajdzSprawdz(N,G),
    jestPWyborem(AE,G).


% upewniamy sie, ze otrzymany graf wyboru ma posortowana liste wierzcholkow

redukujRepr2(G) :-
    sort(G, Gs),
    G = Gs.


% funkcja z zadania

jestWyborem(AE,G) :-
    poprawnyGraf(AE),
    length(AE,X),
    length(G,X),
    jestPWyborem(AE,G),
    redukujRepr2(G).


% dfs(AE, DoPrzejrzenia, Stos, Przejrzane) - sprawdza, czy DoPrzejrzenia jest
% czesciowo przejrzana lista DFS, przy czym aktualne zaglebienie w grafie
% pamietane jest na Stosie, a Przejrzane zawiera wszystkie odwiedzone
% do tej pory wierzcholki

dfs(_, [], [], [_|_]).

dfs(AE, [F|L], [], []) :- 
    member(F,AE),    
    F = [W|_],
    dfs(AE, L, [F], [W]).

% sprawdzenie, czy kazdy sasiad T odwiedzony

dfs(AE, [], [T|S], P) :- 
    T = [_,_|N], 
    podzbior(N, P), 
    dfs(AE, [], S, P).

% zejscie glebiej w grafie

dfs(AE, [F|L], [T|S], P) :-
    member(F, AE),
    F = [W|_],
    \+ member(W, P),
    T = [_,_|N],
    member(W, N),
    dfs(AE, L, [F,T|S], [W|P]).

% cofniecie sie nizej w grafie

dfs(AE, [F|L], [T|S], P) :-
    member(F, AE),
    F = [W|_],
    \+ member(W, P),
    T = [_,_|N],
    \+ member(W, N),
    podzbior(N, P),
    dfs(AE, [F|L], S, P).


% zwezona(L_Szersza, L_Wezsza) - L_Szersza to L_Wezsza poszerzona o typ a/e
% oraz liste sasiadow wierzcholka

zwezona([], []).

zwezona([[V|_]|LR], [V|L]) :-
    zwezona(LR, L).


% funkcja z zadania, sprawdza, czy L jest poprawnym DFS-em z pierwszego
% wierzcholka grafu AE

jestDFS([], []).

jestDFS(AE, L) :-
    poprawnyGraf(AE),
    AE = [[V|_]|_],
    LR = [[V|_]|_],
    dfs(AE, LR, [], []),
    zwezona(LR, L).


% funkcja z zadania, sprawdza, czy L jest DFS-em w pewnym AE-wyborze z AE
% AE-wybor explicite konstruowany

jestADFS(AE, G, L) :- jestWyborem(AE, G), jestDFS(G,L).

jestADFS(AE, L) :- jestADFS(AE, _, L).


% podobnie jak DFS, przy czym dodatkowo pamietany jest parametr pop/push
% jezeli schodzimy glebiej w grafie, to "ustawiamy" go na push
% jezeli cofamy sie, to na pop
% parametr ten jest uzywany do odpowiedniej obslugi wierzcholkow typu 'e'
% jezeli raz weszlismy glebiej z takiego wierzcholka, to przy cofnieciu do niego
% musimy cofac sie nizej,
% przynajmniej jeden sasiad (o ile jakis istnieje) wierzcholka typu e musi byc odwiedzony,
% jezeli kolejny wierzcholek w przejsciu DFS nie jest jego sasiadem

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


% funkcja z tresci zadania, nie konstruuje explicite AE-wyboru

jestADFS1([], []).

jestADFS1(AE, L) :-
    poprawnyGraf(AE),
    AE = [[V|_]|_],
    LR = [[V|_]|_],
    adfs(AE, LR, [], [], push),
    zwezona(LR, L).
