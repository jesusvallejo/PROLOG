
:- use_module(library(stream_utils)).
:- use_module(library(write)).
:- use_module(library(lists)).

nat(0).               
nat(s(X)) :- nat(X).

igual(X,X).

mayorZero(s(0)).               
mayorZero(s(X)) :- mayorZero(X).


lengthEq(X,Y):-lengthEqCom(X,Y,0,0).
lengthEqCom([],[],_,_).
lengthEqCom([A|B],[C|D],Z,P):- igual(Z,P),lengthEqCom(B,D,s(Z),s(P)).


array([[_]]).
array([[_]|[_]]).
array([[_|_]|[_|_]]).
%------------------------  basic building type --------------------------------
basic_building(X):- array(X),basic_building1(X).%miro que no este vacio, para poder hacer despues la recursividad
basic_building1([]). % write_string("fin basic\n"),
basic_building1([X|T]) :- basic_building_comp(X,T). % write_string("iteracion piso\n"),
basic_building_comp([],K):-basic_building1(K).% write_string("fin piso, siguin\n"),
basic_building_comp([Z|C],K):-nat(Z),basic_building_comp(C,K). % write_string("iteracion, vivienda\n"),
%------------------------  basic building predicate tests ---------------------------
%basic_building([[s(0)],[0]]).yes
%basic_building([[s(0),0],[0,s(0)]]). yes
%basic_building([[s(0),s(s(s(0)))],[0,s(s(0))]]). yes
%basic_building([[0]]). yes
%basic_building([[0],0]). no
%basic_building([0]).no
%basic_building(0).no
%basic_building([[s(0)],s(0)]). no
%basic_building([s(0)]).no
%basic_building(s(0)).no
%------------------------- building type ----------------------------------

 
building(X):-basic_building(X),checkLength(X).

%checkLength([]).
checkLength([_]).
checkLength([X,Y|T]):- lengthEq(X,Y),checkLength([Y|T]).
%------------------------ building predicate tests ---------------------------

%building([[0,s(0)],[0,0],[s(0),0],[0,0]]). yes
%building([[0,s(0)],[0,0],[s(0),0],[0]]).   no

%--------------------------  level predicate ---------------------------------------------------
level(X,N,C):-level1(X,N,C,s(0)).
level1(X,0,C,0):-write_string("C = "),write(C).
level1([X|T],N,C,N):- level1(T,0,X,0).%write_string("C = "),write(C).
level1([X|T],N,C,Z):-level1(T,N,X,s(Z)).

%--------------------------  test level predicate ---------------------------------------------------
% level([[ s(0), s(s(s(0))) ], [ 0, s(s(0)) ] ], s(s(0)), C). C=[ 0, s(s(0)) ]
 % level([[ s(0), s(s(s(0))) ], [ 0, s(s(0)) ] ], s(0), C).  C= [ s(0), s(s(s(0)))]
  % level([[ s(0), s(s(s(0))) ], [ 0, s(s(0)),s(s(0))],[ s(0), s(0)]], s(s(s(0))), C).  C= [ s(0), s(0)]

%-----------------------------------------------------------------------------------------------

%--------------------------  column predicate ---------------------------------------------------
column(X,N,C):-column1(X,N,C).
column1([],N,C):- write_string("C = "),write(C).
column1([X|T],N,C):-column_comp(X,N,C,s(0),T).
column_comp([X|Y],N,C,N,T):-column1(T,N,[C,X]).
column_comp([X|Y],N,C,P,T):-column_comp(Y,N,C,s(P),T).

%column([[ s(0),s(s(s(0)))], [0,s(s(0))]],s(0),C).