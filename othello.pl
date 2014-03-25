/* ----------------------------------------------------------
    CSC384 Assignment 2 

% Surname: Teng
% First Name: Fei
% Student Number: 996842751 

  ------------------------------------------------------ */

%do not chagne the follwoing line!
:- use_module(library(lists)).
:- ensure_loaded('play.pl').
:- use_module(library(sort)).


% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers (feel free to add your own helpers if needed,
%       MAKE SURE to write comments for all your helpers, marks will
%       be deducted for bad style!).
%
%       Implement the following predicates at their designated space
%       in this file (we suggest to have a look at file ttt.pl  to
%       see how the implementations is done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr).
%          * winner(State,Plyr) 
%          * tie(State)
%          * terminal(State) 
%          * moves(Plyr,State,MvList)
%          * nextState(Plyr,Move,State,NewState,NextPlyr)
%          * validmove(Plyr,State,Proposed)
%          * h(State,Val)  (see question 2 in the handout)
%          * lowerBound(B)
%          * upperBound(B)
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a piece in this position
%    2 means player two has a piece in this position. 



% given helper: Inital state of the board 
initBoard([ [.,1,.,1,.,.], 
            [1,2,1,2,.,.],
	    	[2,.,2,2,.,.], 
	    	[.,.,2,2,1,.], 
            [1,.,2,2,2,1], 
	    	[.,.,.,.,.,1] ]).
/*initBoard([ [1,2,1,1,1,1], 
            [2,1,1,2,2,1],
	    	[2,1,1,2,1,1], 
	    	[1,1,2,1,1,1], 
            [1,2,1,1,1,1], 
	    	[1,2,2,1,1,1] ]).
*/ 
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 
initialize([[.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    	[.,.,1,2,.,.], 
	    	[.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    	[.,.,.,.,.,.] ],1).%by convention, player 1 to go first

/*initialize([[1,2,1,1,1,1], 
            [2,1,2,2,2,2],
	    	[1,2,1,1,1,1], 
	    	[2,1,2,2,2,2], 
            [1,2,1,1,1,1], 
	    	[2,1,2,2,2,2]],1).*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 
winner(B,1):- % the board is filled and some1 has score more than 18.
	board_is_full(B),
	count1(B,N),
	count2(B,M),N>M.
winner(B,2):-
	board_is_full(B),
	count1(B,N),
	count2(B,M),N<M.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 
tie(B) :- \+ % the board is filled with both player having score = 18.
	winner(B,_),
	board_is_full(B).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   
terminal(State) :- winner(State,_).
terminal(State) :- tie(State).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%% 
showState( G ) :- 
	printRows( G ). 
 
printRows( [] ). 
printRows( [H|L] ) :- 
	printList(H),
	nl,
	printRows(L). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%

moves(1,State,Y):- findall(X, validmove(1,State,X),Mvliste),predsort(compareSecond, Mvliste, X),predsort(compareFirst,X,Y).
moves(2,State,Y):- findall(X, validmove(2,State,X),Mvliste),predsort(compareSecond, Mvliste, X),predsort(compareFirst,X,Y).

%helper used to sort the legal moves in the right order
compareSecond(Delta, [_, A], [_, B]):-
        A == B;compare(Delta, A, B).

compareFirst(Gamma,[A,C],[B,D]):-
        A == B,compare(Gamma, C, D);compare(Gamma, A, B).

%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).

nextState(1,Moves,State,NewState,2):- replPos(1,State,Moves,NewState).
nextState(2,Moves,State,NewState,1):- replPos(2,State,Moves,NewState).

replPos(X,St,Mark,Nst):-% set(Board, NewBoard, [R, C], Value):
	set(St,NstN,Mark,X),%set that spot
	flip_necesssary(Mark,NstN,X,Nst).
%-----------------------------for user 1-----%
flip_necesssary([R,C],B,1,N):-    %flip necesssary spots in the bottom-direction
	flip_bot(1,B,[R+1,C],N),get(B,[Y,C],2),Y is R+1.
flip_bot(1,B,[X,C],N):-
	 get(B,[Y,C],1),Y is X+1,set(B,N,[X,C],1). %check for "121"
flip_bot(1,B,[X,C],N1):-
	get(B,[Y,C],2),Y is X+1,get(B,[Z,C],1),Z is X+2,set(B,N,[X,C],1),set(N,N1,[Y,C],1).  %check for "1221"
flip_bot(1,B,[X,C],N2):-
	get(B,[Y,C],2),Y is X+1,get(B,[Y1,C],2),Y1 is X+2,get(B,[Z,C],1),Z is X+3,set(B,N,[X,C],1),set(N,N1,[Y,C],1),set(N1,N2,[Y1,C],1).  %check for "12221"
flip_bot(1,B,[X,C],N3):-
	get(B,[Y,C],2),Y is X+1,get(B,[Y1,C],2),Y1 is X+2,get(B,[Y2,C],2),Y2 is X+3,get(B,[Z,C],1),Z is X+4,
	set(B,N,[Y,C],1),set(N,N1,[Y1,C],1),set(N1,N2,[Y2,C],1),set(N2,N3,[X,C],1).  %check for "122221"	


flip_necesssary([R,C],B,1,N):-    %flip necesssary spots in the top-direction
	flip_top(1,B,[R-1,C],N),get(B,[Y,C],2),Y is R-1.
flip_top(1,B,[X,C],N):-
	 get(B,[Y,C],1),Y is X-1,set(B,N,[X,C],1). %check for "121"
flip_top(1,B,[X,C],N1):-
	get(B,[Y,C],2),Y is X-1,get(B,[Z,C],1),Z is X-2,set(B,N,[X,C],1),set(N,N1,[Y,C],1).  %check for "1221"
flip_top(1,B,[X,C],N2):-
	get(B,[Y,C],2),Y is X-1,get(B,[Y1,C],2),Y1 is X-2,get(B,[Z,C],1),Z is X-3,set(B,N,[X,C],1),set(N,N1,[Y,C],1),set(N1,N2,[Y1,C],1).  %check for "12221"
flip_top(1,B,[X,C],N3):-
	get(B,[Y,C],2),Y is X-1,get(B,[Y1,C],2),Y1 is X-2,get(B,[Y2,C],2),Y2 is X-3,get(B,[Z,C],1),Z is X-4,
	set(B,N,[Y,C],1),set(N,N1,[Y1,C],1),set(N1,N2,[Y2,C],1),set(N2,N3,[X,C],1).  %check for "122221"	


flip_necesssary([R,C],B,1,N):-    %flip necesssary spots in the right-direction
	flip_right(1,B,[R,C+1],N),get(B,[R,Y],2),Y is C+1.
flip_right(1,B,[R,X],N):-
	 get(B,[R,Y],1),Y is X+1,set(B,N,[R,X],1). %check for "121"
flip_right(1,B,[R,X],N1):-
	get(B,[R,Y],2),Y is X+1,get(B,[R,Z],1),Z is X+2,set(B,N,[R,X],1),set(N,N1,[R,Y],1).  %check for "1221"
flip_right(1,B,[R,X],N2):-
	get(B,[R,Y],2),Y is X+1,get(B,[R,Y1],2),Y1 is X+2,get(B,[R,Z],1),Z is X+3,set(B,N,[R,X],1),set(N,N1,[R,Y],1),set(N1,N2,[R,Y1],1).  %check for "12221"
flip_right(1,B,[R,X],N3):-
	get(B,[R,Y],2),Y is X+1,get(B,[R,Y1],2),Y1 is X+2,get(B,[R,Y2],2),Y2 is X+3,get(B,[R,Z],1),Z is X+4,
	set(B,N,[R,Y],1),set(N,N1,[R,Y1],1),set(N1,N2,[R,Y2],1),set(N2,N3,[R,X],1).  %check for "122221"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.

validmove(1,B,[R,C]):- 
	get(B,[R,C],'.'), %check bot
	get(B,[X,C],2),
	X is R+1,checkbot(1,B,[X,C]).
checkbot(1,B,[X,C]):-
	 (get(B,[Y,C],1),Y is X+1). %check for "121"
checkbot(1,B,[X,C]):-
	get(B,[Y,C],2),Y is X+1,get(B,[Z,C],1),Z is X+2.  %check for "1221"
checkbot(1,B,[X,C]):-
	get(B,[Y,C],2),Y is X+1,get(B,[Y1,C],2),Y1 is X+2,get(B,[Z,C],1),Z is X+3.  %check for "12221"
checkbot(1,B,[X,C]):-
	get(B,[Y,C],2),Y is X+1,get(B,[Y1,C],2),Y1 is X+2,get(B,[Y2,C],2),Y2 is X+3,get(B,[Z,C],1),Z is X+4.  %check for "122221"


validmove(1,B,[R,C]):- 
	get(B,[R,C],'.'), %check up
	get(B,[X,C],2),
	X is R-1,checkup(1,B,[X,C]).
checkup(1,B,[X,C]):-
	 (get(B,[Y,C],1),Y is X-1). %check for "121"
checkup(1,B,[X,C]):-
	get(B,[Y,C],2),Y is X-1,get(B,[Z,C],1),Z is X-2.  %check for "1221"
checkup(1,B,[X,C]):-
	get(B,[Y,C],2),Y is X-1,get(B,[Y1,C],2),Y1 is X-2,get(B,[Z,C],1),Z is X-3.  %check for "12221"
checkup(1,B,[X,C]):-
	get(B,[Y,C],2),Y is X-1,get(B,[Y1,C],2),Y1 is X-2,get(B,[Y2,C],2),Y2 is X-3,get(B,[Z,C],1),Z is X-4.  %check for "122221"

validmove(1,B,[R,C]):- 
	get(B,[R,C],'.'), %check left
	get(B,[R,X],2),
	X is C-1,checkleft(1,B,[R,X]).
checkleft(1,B,[R,X]):-
	 (get(B,[R,Y],1),Y is X-1). %check for "121"
checkleft(1,B,[R,X]):-
	get(B,[R,Y],2), Y is X-1,get(B,[R,Z],1),Z is X-2.  %check for "1221"
checkleft(1,B,[R,X]):-
	get(B,[R,Y],2),Y is X-1,get(B,[R,Y1],2),Y1 is X-2,get(B,[R,Z],1),Z is X-3.  %check for "12221"
checkleft(1,B,[R,X]):-
	get(B,[R,Y],2),Y is X-1,get(B,[R,Y1],2),Y1 is X-2,get(B,[R,Y2],2),Y2 is X-3,get(B,[R,Z],1),Z is X-4.  %check for "122221"	

validmove(1,B,[R,C]):- 
	get(B,[R,C],'.'), %check right
	get(B,[R,X],2),
	X is C+1,checkleft(1,B,[R,X]).
checkright(1,B,[R,X]):-
	 (get(B,[R,Y],1),Y is X+1). %check for "121"
checkright(1,B,[R,X]):-
	get(B,[R,Y],2), Y is X+1,get(B,[R,Z],1),Z is X+2.  %check for "1221"
checkright(1,B,[R,X]):-
	get(B,[R,Y],2),Y is X+1,get(B,[R,Y1],2),Y1 is X+2,get(B,[R,Z],1),Z is X+3.  %check for "12221"
checkright(1,B,[R,X]):-
	get(B,[R,Y],2),Y is X+1,get(B,[R,Y1],2),Y1 is X+2,get(B,[R,Y2],2),Y2 is X+3,get(B,[R,Z],1),Z is X+4.  %check for "122221"	


validmove(1,B,[R,C]):- 
	get(B,[R,C],'.'), %check diagnal_upperleft
	get(B,[Y,X],2),
	X is C-1,Y is R-1,checkDiag_upperleft(1,B,[Y,X]).
checkDiag_upperleft(1,B,[R,C]):-
	 (get(B,[Y,X],1),Y is R-1,X is C-1). %check for "121"
checkDiag_upperleft(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R-1,X is C-1,get(B,[Y1,X1],1),Y1 is R-2,X1 is C-2.  %check for "1221"
checkDiag_upperleft(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R-1,X is C-1,get(B,[Y1,X1],2),Y1 is R-2,X1 is C-2,get(B,[Y2,X2],1),Y2 is R-3,X2 is C-3.  %check for "12221"
checkDiag_upperleft(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R-1,X is C-1,get(B,[Y1,X1],2),Y1 is R-2,X1 is C-2,get(B,[Y2,X2],2),Y2 is R-3,X2 is C-3,get(B,[Y3,X3],1),Y3 is R-4,X3 is C-4.  %check for "122221"	


validmove(1,B,[R,C]):- 
	get(B,[R,C],'.'), %check diagnal_upperright
	get(B,[Y,X],2),
	X is C+1,Y is R-1,checkDiag_upperleft(1,B,[Y,X]).
checkDiag_upperright(1,B,[R,C]):-
	 (get(B,[Y,X],1),Y is R-1,X is C+1). %check for "121"
checkDiag_upperright(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R-1,X is C+1,get(B,[Y1,X1],1),Y1 is R-2,X1 is C+2.  %check for "1221"
checkDiag_upperright(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R-1,X is C+1,get(B,[Y1,X1],2),Y1 is R-2,X1 is C+2,get(B,[Y2,X2],1),Y2 is R-3,X2 is C+3.  %check for "12221"
checkDiag_upperright(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R-1,X is C+1,get(B,[Y1,X1],2),Y1 is R-2,X1 is C+2,get(B,[Y2,X2],2),Y2 is R-3,X2 is C+3,get(B,[Y3,X3],1),Y3 is R-4,X3 is C+4.  %check for "122221"	


validmove(1,B,[R,C]):- 
	get(B,[R,C],'.'), %check diagnal_botleft	
	get(B,[Y,X],2),
	X is C-1,Y is R+1,checkDiag_botleft(1,B,[Y,X]).
checkDiag_botleft(1,B,[R,C]):-
	 (get(B,[Y,X],1),Y is R+1,X is C-1). %check for "121"
checkDiag_botleft(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R+1,X is C-1,get(B,[Y1,X1],1),Y1 is R+2,X1 is C-2.  %check for "1221"
checkDiag_botleft(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R+1,X is C-1,get(B,[Y1,X1],2),Y1 is R+2,X1 is C-2,get(B,[Y2,X2],1),Y2 is R+3,X2 is C-3.  %check for "12221"
checkDiag_botleft(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R+1,X is C-1,get(B,[Y1,X1],2),Y1 is R+2,X1 is C-2,get(B,[Y2,X2],2),Y2 is R+3,X2 is C-3,get(B,[Y3,X3],1),Y3 is R+4,X3 is C-4.  %check for "122221"	


validmove(1,B,[R,C]):- 
	get(B,[R,C],'.'), %check diagnal_botright
	get(B,[Y,X],2),
	X is C+1,Y is R+1,checkDiag_botright(1,B,[Y,X]).
checkDiag_botright(1,B,[R,C]):-
	 (get(B,[Y,X],1),Y is R+1,X is C+1). %check for "121"
checkDiag_botright(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R+1,X is C+1,get(B,[Y1,X1],1),Y1 is R+2,X1 is C+2.  %check for "1221"
checkDiag_botright(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R+1,X is C+1,get(B,[Y1,X1],2),Y1 is R+2,X1 is C+2,get(B,[Y2,X2],1),Y2 is R+3,X2 is C+3.  %check for "12221"
checkDiag_botright(1,B,[R,C]):-
	get(B,[Y,X],2),Y is R+1,X is C+1,get(B,[Y1,X1],2),Y1 is R+2,X1 is C+2,get(B,[Y2,X2],2),Y2 is R+3,X2 is C+3,get(B,[Y3,X3],1),Y3 is R+4,X3 is C+4.  %check for "122221"	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validmove(2,B,[R,C]):- 
	get(B,[R,C],'.'), %check bot
	get(B,[X,C],1),
	X is R+1,checkbot(2,B,[X,C]).
checkbot(2,B,[X,C]):-
	 (get(B,[Y,C],1),Y is X+1). %check for "121"
checkbot(2,B,[X,C]):-
	get(B,[Y,C],1),Y is X+1,get(B,[Z,C],2),Z is X+2.  %check for "1221"
checkbot(2,B,[X,C]):-
	get(B,[Y,C],1),Y is X+1,get(B,[Y1,C],1),Y1 is X+2,get(B,[Z,C],2),Z is X+3.  %check for "12221"
checkbot(2,B,[X,C]):-
	get(B,[Y,C],1),Y is X+1,get(B,[Y1,C],1),Y1 is X+2,get(B,[Y2,C],1),Y2 is X+3,get(B,[Z,C],2),Z is X+4.  %check for "122221"


validmove(2,B,[R,C]):- 
	get(B,[R,C],'.'), %check up
	get(B,[X,C],1),
	X is R-1,checkup(2,B,[X,C]).
checkup(2,B,[X,C]):-
	 (get(B,[Y,C],2),Y is X-1). %check for "121"
checkup(2,B,[X,C]):-
	get(B,[Y,C],1),Y is X-1,get(B,[Z,C],2),Z is X-2.  %check for "1221"
checkup(2,B,[X,C]):-
	get(B,[Y,C],1),Y is X-1,get(B,[Y1,C],1),Y1 is X-2,get(B,[Z,C],2),Z is X-3.  %check for "12221"
checkup(2,B,[X,C]):-
	get(B,[Y,C],1),Y is X-1,get(B,[Y1,C],1),Y1 is X-2,get(B,[Y2,C],1),Y2 is X-3,get(B,[Z,C],2),Z is X-4.  %check for "122221"

validmove(2,B,[R,C]):- 
	get(B,[R,C],'.'), %check left
	get(B,[R,X],1),
	X is C-1,checkleft(1,B,[R,X]).
checkleft(2,B,[R,X]):-
	 (get(B,[R,Y],2),Y is X-1). %check for "121"
checkleft(2,B,[R,X]):-
	get(B,[R,Y],1), Y is X-1,get(B,[R,Z],2),Z is X-2.  %check for "1221"
checkleft(2,B,[R,X]):-
	get(B,[R,Y],1),Y is X-1,get(B,[R,Y1],1),Y1 is X-2,get(B,[R,Z],2),Z is X-3.  %check for "12221"
checkleft(2,B,[R,X]):-
	get(B,[R,Y],1),Y is X-1,get(B,[R,Y1],1),Y1 is X-2,get(B,[R,Y2],1),Y2 is X-3,get(B,[R,Z],2),Z is X-4.  %check for "122221"	

validmove(2,B,[R,C]):- 
	get(B,[R,C],'.'), %check right
	get(B,[R,X],1),
	X is C+1,checkleft(1,B,[R,X]).
checkright(2,B,[R,X]):-
	 (get(B,[R,Y],2),Y is X+1). %check for "121"
checkright(2,B,[R,X]):-
	get(B,[R,Y],1), Y is X+1,get(B,[R,Z],2),Z is X+2.  %check for "1221"
checkright(2,B,[R,X]):-
	get(B,[R,Y],1),Y is X+1,get(B,[R,Y1],1),Y1 is X+2,get(B,[R,Z],2),Z is X+3.  %check for "12221"
checkright(2,B,[R,X]):-
	get(B,[R,Y],1),Y is X+1,get(B,[R,Y1],1),Y1 is X+2,get(B,[R,Y2],1),Y2 is X+3,get(B,[R,Z],2),Z is X+4.  %check for "122221"	


validmove(2,B,[R,C]):- 
	get(B,[R,C],'.'), %check diagnal_upperleft
	get(B,[Y,X],1),
	X is C-1,Y is R-1,checkDiag_upperleft(2,B,[Y,X]).
checkDiag_upperleft(2,B,[R,C]):-
	 (get(B,[Y,X],2),Y is R-1,X is C-1). %check for "121"
checkDiag_upperleft(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R-1,X is C-1,get(B,[Y1,X1],2),Y1 is R-2,X1 is C-2.  %check for "1221"
checkDiag_upperleft(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R-1,X is C-1,get(B,[Y1,X1],1),Y1 is R-2,X1 is C-2,get(B,[Y2,X2],2),Y2 is R-3,X2 is C-3.  %check for "12221"
checkDiag_upperleft(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R-1,X is C-1,get(B,[Y1,X1],1),Y1 is R-2,X1 is C-2,get(B,[Y2,X2],1),Y2 is R-3,X2 is C-3,get(B,[Y3,X3],2),Y3 is R-4,X3 is C-4.  %check for "122221"	


validmove(2,B,[R,C]):- 
	get(B,[R,C],'.'), %check diagnal_upperright
	get(B,[Y,X],1),
	X is C+1,Y is R-1,checkDiag_upperleft(2,B,[Y,X]).
checkDiag_upperright(2,B,[R,C]):-
	 (get(B,[Y,X],2),Y is R-1,X is C+1). %check for "121"
checkDiag_upperright(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R-1,X is C+1,get(B,[Y1,X1],2),Y1 is R-2,X1 is C+2.  %check for "1221"
checkDiag_upperright(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R-1,X is C+1,get(B,[Y1,X1],1),Y1 is R-2,X1 is C+2,get(B,[Y2,X2],2),Y2 is R-3,X2 is C+3.  %check for "12221"
checkDiag_upperright(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R-1,X is C+1,get(B,[Y1,X1],1),Y1 is R-2,X1 is C+2,get(B,[Y2,X2],1),Y2 is R-3,X2 is C+3,get(B,[Y3,X3],2),Y3 is R-4,X3 is C+4.  %check for "122221"	


validmove(2,B,[R,C]):- 
	get(B,[R,C],'.'), %check diagnal_botleft	
	get(B,[Y,X],1),
	X is C-1,Y is R+1,checkDiag_botleft(2,B,[Y,X]).
checkDiag_botleft(2,B,[R,C]):-
	 (get(B,[Y,X],2),Y is R+1,X is C-1). %check for "121"
checkDiag_botleft(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R+1,X is C-1,get(B,[Y1,X1],2),Y1 is R+2,X1 is C-2.  %check for "1221"
checkDiag_botleft(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R+1,X is C-1,get(B,[Y1,X1],1),Y1 is R+2,X1 is C-2,get(B,[Y2,X2],2),Y2 is R+3,X2 is C-3.  %check for "12221"
checkDiag_botleft(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R+1,X is C-1,get(B,[Y1,X1],1),Y1 is R+2,X1 is C-2,get(B,[Y2,X2],1),Y2 is R+3,X2 is C-3,get(B,[Y3,X3],2),Y3 is R+4,X3 is C-4.  %check for "122221"	


validmove(2,B,[R,C]):- 
	get(B,[R,C],'.'), %check diagnal_botright
	get(B,[Y,X],1),
	X is C+1,Y is R+1,checkDiag_botright(2,B,[Y,X]).
checkDiag_botright(2,B,[R,C]):-
	 (get(B,[Y,X],2),Y is R+1,X is C+1). %check for "121"
checkDiag_botright(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R+1,X is C+1,get(B,[Y1,X1],2),Y1 is R+2,X1 is C+2.  %check for "1221"
checkDiag_botright(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R+1,X is C+1,get(B,[Y1,X1],1),Y1 is R+2,X1 is C+2,get(B,[Y2,X2],2),Y2 is R+3,X2 is C+3.  %check for "12221"
checkDiag_botright(2,B,[R,C]):-
	get(B,[Y,X],1),Y is R+1,X is C+1,get(B,[Y1,X1],1),Y1 is R+2,X1 is C+2,get(B,[Y2,X2],1),Y2 is R+3,X2 is C+3,get(B,[Y3,X3],2),Y3 is R+4,X3 is C+4.  %check for "122221"	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(S,0):- \+ terminal(S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.
lowerBound(-100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.

upperBound(100).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position row R column C
% set(Board, NewBoard, [R, C], Value):
%    : set Value at row R column C in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [0,5], the lower left
% hand corner has index [5,0], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [3,2], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 	
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [4,2], 1), set(NB1, NB2, [3,2], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . initBoard(B)
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%initBoard(B)
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% get(Board, Point, Element): get the value of the board at position
% row R column C (indexing starts at 0).
get( Board, [R, C], Value) :- 
	nth0( R, Board, Row), 
	nth0( C, Row, Value).
 
% set( Board, NewBoard, [X, Y], Value) 

set( [Row|RestRows], [NewRow|RestRows], [0, C], Value)
    :- setInList(Row, NewRow, C, Value). 

set( [Row|RestRows], [Row|NewRestRows], [R, C], Value) :- 
	R > 0, 
	R1 is R-1, 
	set( RestRows, NewRestRows, [R1, C], Value). 

% setInList( List, NewList, Index, Value) 

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
%%%%%%%%%%%%%%%%%%%%%%%%HELPERS%%%%%%%%%%%%%%%%%% 
% notmember(E,L) is true if E is not a member of list L
notmember(_,[]).
notmember(N,[H|T]) :- N \= H, notmember(N,T).
%board_is_full detects if the State is a terminal state.
board_is_full(B):- \+get(B,[X,Y],'.').


%count1 and count2 are helper functions to determine the score of each player
count2(B,Num):-
	flatten(B,G),count(2,G,Num).

count1(B,Num):-
	flatten(B,G),count(1,G,Num).

count(_,[],0).
         	count(A,[A|L],N):- !,count(A,L,N1),N is N1+1.
         	count(A,[_|L],N):- count(A,L,N). 