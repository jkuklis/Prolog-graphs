test(ok).

nat(0).
nat(s(X)) :- nat(X).
even(0).
even(s(s(X))) :- even(X).

tree(empty).
tree(node(L, _, R)) :- tree(L), tree(R).

isoTree(empty, empty).
isoTree(node(L1, X, R1), node(L2, X, R2)) :- isoTree(L1, L2), isoTree(R1, R2).

% dziecko(Dziecko, Matka, Ojciec)
dziecko(jasio, ewa, jan).
dziecko(stasio, ewa, jan).
dziecko(basia, anna, piotr).
dziecko(jan, ela, jakub).

% ojciec(Dziecko, Ojciec)
ojciec(X,Y) :- dziecko(X,_,Y).

% przodek(Przodek, Potomek)
przodek(X,Y) :- dziecko(Y,X,_).
przodek(X,Y) :- dziecko(Y,_,X).
przodek(X,Y) :- dziecko(Y,Z,_), przodek(X,Z).
przodek(X,Y) :- dziecko(Y,_,Z), przodek(X,Z).

% lista(Lista)
lista([]).
lista([_|_]).

% pierwszy(Elt, Lista)
pierwszy(E, [E|_]).

% ostatni(Elt, Lista)
ostatni(E,[F|L]) :- E = F, L = [].
ostatni(X,[_|L]) :- ostatni(X,L).

% element(Elt, Lista)
element(X,[Y|_]) :- X = Y.
element(X,[_|L]) :- element(X,L).

scal([], L2, L2).
scal(L1, [], L1).
scal([X|L1], L2, [X|L3]) :- scal(L1, L2, L3).

podziel(L,NP,P) :- podziel(L,NP,P,1).
podziel([X|L],[X|NP],P,1) :- podziel(L,NP,P,0).
podziel([X|L],NP,[X|P],0) :- podziel(L,NP,P,1).
podziel([],[],[],0).
podziel([],[],[],1).

pisz([]).
pisz([X|Y]):-write(X),nl,pisz(Y).

srodek(E, [E]).
srodek(E, [E,_]).
