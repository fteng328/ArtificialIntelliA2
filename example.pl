elephant(fred).
elephant(mary).
elephant(joe).
animal(fred) :- elephant(fred). animal(mary) :- elephant(mary). animal(joe) :- elephant(joe).


edge(a,b).
edge(a,c).
edge(b,d).
edge(d,e).
path(X,Y) :- edge(X,Y).
path(X,Y) :- edge(X,Z), path(Z,Y).

member(X,[X|_]).
member(X,[Y|T]):-X \= Y, member(X,T).

build(I,J,[ ]) :-I > J.
build(I,J,[I | Rest]) :-
	I < J, N is I + 1, build(N,J,Rest).

size([],0).
size([ _|T],N) :-size(T,N1), N is N1+1.




row(M, N, Row) :-
    nth1(N, M, Row).

column(M, N, Col) :-
    transpose(M, MT),
    row(MT, N, Col).

symmetrical(M) :-
    transpose(M, M).

transpose([[]|_], []) :- !.
transpose([[I|Is]|Rs], [Col|MT]) :-
    first_column([[I|Is]|Rs], Col, [Is|NRs]),
    transpose([Is|NRs], MT).

first_column([], [], []).
first_column([[]|_], [], []).
first_column([[I|Is]|Rs], [I|Col], [Is|Rest]) :-
    first_column(Rs, Col, Rest).


counter(Y,X):-
	write(X),write(Y),X = '.'.



%board_is_full is true if board is board_is_full

isNotEqual(A,B):- A\=B.


count1s(State,X):-
countinrows(State,X).
 
countinrows( [],A ). 
countinrows( [H|L],Count ) :- 
	countinlists(H),
	nl,
	countinrows(L). 

countinlists([]).
countinlists([H | L]) :-
	write(H),
	write(' '),
	countinlists(L).

count([],X,0).
count([X|T],X,Y):- count(T,X,Z), Y is 1+Z.
count([X1|T],X,Z):- X1\=X,count(T,X,Z).

countall(List,X,C) :-
    sort(List,List1),
    member(X,List1),
    count(List,X,C).


%board_is_full(B,Count):-
%	lookRows(B,Count).%,Count = 4.
%lookRows([],0).
%lookRows([H/L],Count):-
%	lookList(H,M),lookRows(L,N),Count is M+N.
%lookList([],0).
%lookList([H/L],Count):- 
%	
%	lookList(L,Sum) ,
%	Count is Sum+1.


%
%showState( G ) :- 
%	printRows( G ). 
 
%printRows( [] ). 
%printRows( [H|L] ) :- 
%	printList(H),
%	nl,
%	printRows(L). 

%printList([]).
%printList([H | L]) :-
%	write(H),
%	write(' '),
%	printList(L).


% 1 terminating condition
     %count_elems([], 0).
    % % 2 recursive
 %    count_elems([Head|Tail], Count) :-
 %         count_elems(Tail, Sum), 
   %       Count is Sum + 1.
%
%
  %        len([],0).
 %         len([H|T],N) :-
  %        sum(H,L),
 %         len(T,M), N is M+1+L.
%
 %         sum([],0).
%          sum([_|T],N) :-
%          sum(T,M), N is M+1.
%