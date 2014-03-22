%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% CSC384 Introduction to Artificial Intelligence, Winter 2014
% Assignment 2
%
% Instructions for preparing your competition files:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If you decide to participate in the competition, you should
% submit a file named "heuristic_<id>.pl", where <id> should
% be your cdf id. For example, a student John Doe with id "c9jdoe" would
% submit the file named "heuristic_c9jdoe.pl".
% 
% This file should contain a predicate named "<id>_h/2"
% (John would name it "c9jdoe_h"). This predicate
% can rely on the functions you are required to implement (eg. tie, winner,
% nextState etc). Any helper predicates
% should also be included in the heuristic file.
% To avoid name clashes, prefix any helper predicates with your id. Note
% that any calls made to them from this file should
% use the new predicate names.
% 
% You should also duplicate the functions lowerBound/1 and upperBound/1
% in this file, again, prefixing them with your id.
% 
% The following is an example submission by John. He clearly did
% not understand the game and didn't use comments properly,
% but he got the naming convention right!
% 
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%-------------------------
% Surname: Doe
% First Name: John
% Student Number: 123456789

% Helper predicates
c9jdoe_max(C1, C2, C) :- C1 >= C2, !, C=C1.
c9jdoe_max(C1, C2, C2) :- C1 < C2.

c9jdoe_count([], 0).
c9jdoe_count([E|L], C) :- length(E, C1),
     c9jdoe_count(L, C2), c9jdoe_max(C1, C2, C).

% The actual heuristic
c9jdoe_h(State, Val) :- terminal(State), !, Val=42.
c9jdoe_h(State, Val) :- c9jdoe_count(State, Val2), Val is Val2-4.

% The bounds
c9jdoe_lowerBound(-3).
c9jdoe_upperBound(300).

