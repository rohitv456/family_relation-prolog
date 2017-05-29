:- discontiguous male/1, female/1, father/2, mother/2.

male(ajay).
female(neelam).
father(ajay,ankit).
father(ajay,nidhi).
father(ajay,rohit).
mother(neelam,rohit).
mother(neelam,ankit).
mother(neelam,nidhi).
male(rohit).
male(ankit).
female(nidhi).

male(omkar).
female(neelu).
father(omkar,sujeet).
father(omkar,ajeet).
father(omkar,sunita).
father(omkar,amit).
mother(neelu,sujeet).
mother(neelu,ajeet).
mother(neelu,amit).
mother(neelu,sunita).
male(ajeet).
male(sujeet).
male(amit).
female(sunita).

father(ajeet,ria).
female(ria).
mother(nitika,ria).
female(nitika).

father(sujeet,radhika).
female(radhika).
mother(ritika,radhika).
female(ritika).

father(amit,govind).
male(govind).
mother(gitika,govind).
female(gitika).

mother(sunita,sonali).
father(ramesh,sonali).
male(ramesh).
female(sonali).


male(rajkumar).
female(vidya).
father(rajkumar,vineeta).
father(rajkumar,alok).
father(rajkumar,shivangi).
father(rajkumar,radha).
mother(vidya,vineeta).
mother(vidya,alok).
mother(vidya,shivangi).
mother(vidya,radha).
male(alok).
female(radha).
female(vineeta).
female(shivangi).

father(arvind,aditya).
mother(vineeta,aditya).
father(arvind,aradhya).
mother(vineeta,aradhya).
male(arvind).
male(aditya).
female(aradhya).


male(sudhir).
female(ritu).
father(sudhir,santosh).
father(sudhir,chandni).
father(sudhir,prakash).
mother(ritu,santosh).
mother(ritu,chandni).
mother(ritu,prakash).
male(santosh).
male(prakash).
female(chandni).

father(santosh,supriya).
mother(santoshi,supriya).
female(supriya).
female(santoshi).

mother(chandni,chandu).
father(chandrajeet,chandu).
male(chandu).
male(chandrajeet).

male(surendra).
female(ganga).
father(surendra,ajay).
father(surendra,omkar).
father(surendra,rajkumar).
father(surendra,ritu).
mother(ganga,ajay).
mother(ganga,rajkumar).
mother(ganga,omkar).
mother(ganga,ritu).



female(saraswati).
male(sampat).
father(sampat,neelam).
father(sampat,rakesh).
father(sampat,niranjan).
father(sampat,manisha).
father(sampat,pammy).
father(sampat,poonam).
father(sampat,aneeta).
mother(saraswati,neelam).
mother(saraswati,rakesh).
mother(saraswati,niranjan).
mother(saraswati,manisha).
mother(saraswati,pammy).
mother(saraswati,poonam).
mother(saraswati,aneeta).
male(rakesh).
male(niranjan).
female(aneeta).
female(poonam).
female(pammy).
female(manisha).

father(jay,jaykishan).
mother(poonam,jaykishan).
male(jaykishan).
male(jay).
father(jay,neetu).
mother(poonam,neetu).
female(neetu).

father(manish,divya).
mother(pammy,divya).
female(divya).
male(manish).

father(anil,paheli).
mother(aneeta,paheli).
female(paheli).
male(anil).

father(sanjay,sanju).
mother(manisha,sanju).
male(sanjay).
male(sanju).
father(sanjay,vrinda).
mother(manisha,vrinda).
female(vrinda).


father(rakesh,muskaan).
mother(neelima,muskaan).
female(neelima).
female(muskaan).
father(rakesh,ritesh).
mother(neelima,ritesh).
male(ritesh).

father(niranjan,rishu).
mother(suman,rishu).
male(rishu).
female(suman).
father(niranjan,gadeli).
mother(suman,gadeli).
female(gadeli).


ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(P,Y),ancestor(X,P).


descendent(X,Y) :- parent(Y,X).
descendent(X,Y) :- parent(P,X),descendent(P,Y).

femaleancestor(X,Y) :- mother(X,Y).
femaleancestor(X,Y) :- mother(P,Y),ancestor(X,P).

maleancestor(X,Y) :- father(X,Y).
maleancestor(X,Y) :- father(P,Y),ancestor(X,P).



parent(Parent,Person) :- father(Parent,Person),male(Parent).
parent(Parent,Person) :- mother(Parent,Person),female(Parent).



grandparent(Grandparent,Person) :- parent(Grandparent,X),parent(X,Person).

greatgrandparent(Greatgrand,Person) :-  grandparent(Greatgrand,X),parent(X,Person).

grandchildren(X,Y) :- grandparent(Y,X).

greatgrandchildren(X,Y) :- greatgrandparent(Y,X).

pota(X,Y) :-  parent(Y,A),father(A,X),male(X),male(A).

poti(X,Y) :- parent(Y,A),father(A,X),female(X),male(A).

naata(X,Y) :- parent(Y,A),mother(A,X),male(X),female(A).

naati(X,Y) :- parent(Y,A),mother(A,X),female(X),female(A).

daada(X,Y) :- pota(Y,X),male(X).
daada(X,Y) :- poti(Y,X),male(X). 

daadi(X,Y) :- pota(Y,X),female(X).
daadi(X,Y) :- poti(Y,X),female(X).

naana(X,Y) :- naata(Y,X),male(X).
naana(X,Y) :- naati(Y,X),male(X).

naani(X,Y) :- naata(Y,X),female(X).
naani(X,Y) :- naati(Y,X),female(X).

sibling(Sibling,Person) :- parent(P,Sibling),parent(P,Person), Sibling \= Person.

sister(X,Y) :- sibling(X,Y),female(X).

brother(X,Y) :- sibling(X,Y),male(X).

son(X,Y) :-  parent(Y,X),male(X).

daughter(X,Y) :-  parent(Y,X),female(X).



married(X,Y) :- parent(X,Child),parent(Y,Child), X \= Y.

wife(X,Y) :- married(X,Y),female(X),male(Y).

husband(X,Y) :- married(X,Y),male(X),female(Y).


cousin1(Child1,Child2) :-parent(Y1,Child1),parent(Y2,Child2),sibling(Y1,Y2).



chacha(X,Y) :-  brother(X,Z),father(Z,Y),male(Z).
chacha(X,Y) :-  cousin1(X,Z),father(Z,Y),male(Z),male(X).

chachi(X,Y) :-  wife(X,Z),brother(Z,W),father(W,Y),male(W).
chachi(X,Y) :-  wife(X,Z),cousin1(Z,W),father(W,Y),male(W),male(Z).

bua(X,Y) :-  	sister(X,Z),father(Z,Y),male(Z).
bua(X,Y) :- 	cousin1(X,Z),father(Z,Y),male(Z),female(X).

phupha(X,Y) :-  husband(X,Z),sister(Z,K),father(K,Y),male(K).
phupha(X,Y) :- 	husband(X,Z),cousin1(Z,K),father(K,Y),male(K),female(Z).


maama(X,Y) :-	brother(X,K),mother(K,Y),female(K).
maama(X,Y) :-	cousin1(X,Z),mother(Z,Y),female(Z),male(X).
maama(X,Y) :-   brother(X,K),mother(K,Z),cousin1(Z,Y).
	
maami(X,Y) :- 	wife(X,K),brother(K,J),mother(J,Y),female(J).
maami(X,Y) :-	wife(X,Z),maama(Z,K),cousin1(K,Y).
 

mausi(X,Y) :- sister(X,Z),mother(Z,Y),female(Z).
mausi(X,Y) :- cousin1(X,Z),mother(Z,Y),female(X),female(Z).

mausa(X,Y) :- husband(X,K),sister(K,Z),mother(Z,Y),female(Z).
mausa(X,Y) :- husband(X,K),cousin1(K,W),mother(W,Y),female(W),female(K).



bhabhi(X,Y) :-  wife(X,Z),cousin1(Z,Y),female(X),male(Z).
bhabhi(X,Y) :-  wife(X,Z),brother(Z,Y),female(X).


dewar(X,Y) :-  brother(X,Z),wife(Y,Z),male(Z).
dewar(X,Y) :-  cousin1(X,Z),wife(Y,Z),male(X).

dewarani(X,Y) :- husband(K,X),husband(Z,Y),brother(K,Z).
dewarani(X,Y) :- husband(K,X),husband(Z,Y),cousin1(K,Z).

saadhu(X,Y) :- husband(X,K),husband(Y,Z),sibling(K,Z),female(K),female(Z).
saadhu(X,Y) :- husband(X,K),husband(Y,Z),cousin1(K,Z),female(K),female(Z).

jija(X,Y) :- husband(X,Z),sibling(Y,Z).
jija(X,Y) :- husband(X,Z),cousin1(Y,Z).

saali(X,Y) :- sister(X,Z),husband(Y,Z).
saali(X,Y) :- cousin1(X,Z),husband(Y,Z),female(X).

saala(X,Y) :- husband(Y,Z),brother(X,Z).
saala(X,Y) :- husband(Y,Z),cousin1(X,Z),male(X).




zamai(X,Y) :- parent(Y,K),husband(X,K).
zamai(X,Y) :- sibling(Y,Z),parent(Z,K),husband(X,K). 
zamai(X,Y) :- cousin1(Y,Z),parent(Z,K),husband(X,K).


bahu(X,Y) :- parent(Y,Z),wife(X,Z).
bahu(X,Y) :- sibling(Y,Z),parent(Z,K),wife(X,K).
bahu(X,Y) :- cousin1(Y,Z),parent(Z,K),wife(X,K).

part1([who,is,R,of,X],[the,R,of,X,is,Y]):-
	apply(R,[Y,X]).
part2([whose,R,is,X],[X,is,R,of,Y]):-
	apply(R,[X,Y]).

:-use_module(library(lists)).

check([A|R], X, Y, Z) :-
	apply(A, [X, Y]	), 
	Z=A;
	check(R, X, Y, Z).

part3([how, is, X, related, to, Y], [X, is, Y, V], [Z|R]) :-
	bfs(X, Y, A, 0, [Z|R], U), reverse(U, V).

bfs(S,G,L,R, [A|B], Ls) :-
	R == 0,
	asserta(goal_vertex(G)),
	asserta(queue(S,[S])),
	asserta(queue1(S,[])),
	queue(_,_),
	bfs_main(S,G,L, [A|B], Ls);
	R == 1,
	queue(_,_),
	bfs_main(S,G,L, [A|B], Ls);
	queue(_,_),
	bfs(S,G,L,1,[A|B], Ls).

bfs_main(S,G,L, [A|B], Ls):-
	queue(X,La),
	queue1(X,Lb),
	neighbour(X,La,Y, [A|B], Lb),
	goal_vertex(Y),!,
	queue(Y,L2),reverse(L2,L),
	queue1(Y, LL2),reverse(LL2, Ls),
	retractall(queue(_,_)),
	retractall(queue1(_,_)),
	retractall(goal_vertex(_)).

neighbour(X,La,Y, [A|B], Lb):-
	check([A|B], X,Y, Z),
	not(member(Y,La)),
	assertz(queue(Y,[Y|La])),
	assertz(queue1(Y,[Z|Lb]));
	retract(queue(X,La)),
	retract(queue1(X,Lb)),
	fail.




















