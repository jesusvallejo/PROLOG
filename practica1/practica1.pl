#!/usr/bin/env ciao-shell
% -*- mode: ciao; -*-


:- use_module(library(stream_utils)).
:- use_module(library(write)).
:- use_module(library(lists)).

nat(0).               
nat(s(X)) :- nat(X).

igual(X,X).
igual([X],[X]).

mayorZero(s(0)).               
mayorZero(s(X)) :- mayorZero(X).

naive_reverse([],[]).
naive_reverse([X|Xs],Zs) :-
	naive_reverse(Xs,Ys),
	my_append(Ys,[X],Zs).

my_append([],Ys,Ys).
my_append([X|Xs],Ys,[X|Zs]) :-
	my_append(Xs,Ys,Zs).


my_length([],0).
my_length([X|Xs],s(N)) :-
	my_length(Xs,N).

checklist([T]) :-
        checklist(T).




append1([],Ys,[Ys]).
append1([X|Xs],Ys,[X|Zs]) :-append1(Xs,Ys,Zs).


suma(0, X, X):- nat(X).
suma(s(X),Y,s(Z)):-suma(X,Y,Z).



lengthEq(X,Y):-lengthEqCom(X,Y,0,0).% devuelve true si las dos listas tienen la misma longitud
lengthEqCom([],[],_,_).
lengthEqCom([_|B],[_|D],Z,P):- igual(Z,P),lengthEqCom(B,D,s(Z),s(P)).


array([[_]]). % comprueba que las listas del edificio sean listas de listas 
array([[_]|[_]]).
array([[_|_]|[_|_]]).


%------------------------  basic building type --------------------------------
basic_building(X):- array(X),basic_building1(X).%miro que no este vacio, para poder hacer despues la recursividad
basic_building1([]). % write_string("fin basic\n"),
basic_building1([X|T]) :- basic_building_comp(X,T). % write_string("iteracion piso\n"),
basic_building_comp([],K):-basic_building1(K).% write_string("fin piso, siguin\n"),
basic_building_comp([Z|C],K):-nat(Z),basic_building_comp(C,K). % write_string("iteracion, vivienda\n"),
%------------------------  basic building predicate tests ---------------------aaaaaaaws444
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

level([X|Y],N,C):-igual(N,s(0)),igual(C,X);(suma(Z,s(0),N),level(Y,Z,C)).






%--------------------------  test level predicate ---------------------------------------------------
% X=[[ s(0), s(s(s(0))) ], [ 0, s(s(0)) ] ],level(X, s(s(0)), C). C=[ 0, s(s(0)) ]
 % level([[ s(0), s(s(s(0))) ], [ 0, s(s(0)) ] ], s(0), C).  C= [ s(0), s(s(s(0)))]
  % level([[ s(0), s(s(s(0))) ], [ 0, s(s(0)),s(s(0))],[ s(0), s(0)]], s(s(s(0))), C).  C= [ s(0), s(0)]

%-----------------------------------------------------------------------------------------------

%--------------------------  column predicate ---------------------------------------------------
column(X,N,C):-column_aux(X,N,K),naive_reverse(K,C).
column_aux([],N,C).
column_aux([X|Y],N,C):-column_aux(Y,N,I),level(X,N,Cs),append1(I,Cs,C).
%  column([[ s(0),s(s(s(0)))], [s(0),s(s(0))],[s(s(0)),s(s(0))],[0,s(s(0))]],s(s(0)),C).

columns(X,C):-my_length(X,T),columns_aux(X,0,K,T),naive_reverse(K,C).
columns_aux(X,N,C,N).
columns_aux(X,N,C,T):-suma(N,s(0),K),columns_aux(X,K,I,T),column(X,N,O),append1(I,O,C).
% columns([[ s(0),s(s(s(0)))],[s(0),s(s(0))]],C).




total_people([],T).
total_people([X|P],T):- total_people(P,O),total_people_rec(X,S),suma(O,S,T).
total_people_rec([],T).
total_people_rec([X|Y],T):-total_people_rec(Y,O),suma(O,X,T).


% total_people([[ s(0),s(s(s(0)))], [s(0),s(s(0))],[s(s(0)),s(s(0))],[0,s(s(0))]],T).