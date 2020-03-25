
:- use_module(library(stream_utils)).
:- use_module(library(write)).
:- use_module(library(lists)).

nat(0).               
nat(s(X)) :- nat(X).

igual(X,X).

mayorZero(s(0)).               
mayorZero(s(X)) :- mayorZero(X).

append1([],Ys,Ys).
append1([X|Xs],Ys,[X|Zs]) :-
	append1(Xs,Ys,Zs).

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
level([],N,N).
level([X|Y],N,C):-igual(N,s(0)),igual(C,X);(suma(Z,s(0),N),level(Y,Z,C)).






%--------------------------  test level predicate ---------------------------------------------------
% X=[[ s(0), s(s(s(0))) ], [ 0, s(s(0)) ] ],level(X, s(s(0)), C). C=[ 0, s(s(0)) ]
 % level([[ s(0), s(s(s(0))) ], [ 0, s(s(0)) ] ], s(0), C).  C= [ s(0), s(s(s(0)))]
  % level([[ s(0), s(s(s(0))) ], [ 0, s(s(0)),s(s(0))],[ s(0), s(0)]], s(s(s(0))), C).  C= [ s(0), s(0)]

%-----------------------------------------------------------------------------------------------

%--------------------------  column predicate ---------------------------------------------------
%column(X,N,C).
%columnRec([ListaPrincipal|RestoListaPrincipal],N,C) :- columnRec2(ListaPrincipal,N,C,S(0),RestoListaPrincipal).
%columnRec2([],N,C,0,RestoListaPrincipal):- columnRec(RestoListaPrincipal,N,C).
%columnRec2([ListaSecundaria|RestoListaSecundaria],N,C,N,RestoListaPrincipal):- columnRec(RestoListaPrincipal,N,[C,ListaSecundaria]).
%columnRec2([ListaSecundaria|RestoListaSecundaria],N,C,P,RestoListaPrincipal):- columnRec2(RestoListaSecundaria,N,C,S(P),RestoListaPrincipal).
 




%column([[ s(0),s(s(s(0)))], [0,s(s(0))]],s(0),C).