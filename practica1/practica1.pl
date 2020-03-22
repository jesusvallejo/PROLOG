
:- use_module(library(stream_utils)).
:- use_module(library(write)).
:- use_module(library(lists)).

nat(0).               
nat(s(X)) :- nat(X).
igual(X,X).
%------------------------  basic building predicate --------------------------------
basic_building(X):- length(X,N),N>0,basic_building1(X).
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
%------------------------- building   predicate ----------------------------------

 
building(X):-basic_building(X),checkLength(X).

checkLength([]).
checkLength([_]).
checkLength([X,Y|T]):- length(X,N),length(Y,N1),igual(N,N1),
						checkLength([Y|T]).
%building([[0,s(0)],[0,0],[s(0),0],[0,0]]). yes
%building([[0,s(0)],[0,0],[s(0),0],[0]]).   no