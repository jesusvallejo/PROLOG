:-module(_,_,[assertions]).

alumno_prode('Vallejo','Collados','Jesus','X150319').
alumno_prode('Rubio','Martin','Roberto','w140035').
alumno_prode('Montanez','Soria','Diego','X150336').

nat(0).  %nat(X),comprueba que  X es un umero natural             
nat(s(X)) :- nat(X).

igual(X,X). %comprueba que igual(A,B) que A y B son iguales.
igual([X],[X]).

naive_reverse([],[]).% da la vuelta a una lista
naive_reverse([X|Xs],Zs) :-
	naive_reverse(Xs,Ys),
	my_append(Ys,[X],Zs).

my_append([],Ys,Ys).%append necesario para sar la vuelta a una lista, se diferencia de append1 , en que no crea una lista de listas.
my_append([X|Xs],Ys,[X|Zs]) :-
	my_append(Xs,Ys,Zs).

my_length([],0).%Calcula el tamanio de una lista
my_length([X|Xs],s(N)) :-
	my_length(Xs,N).
        
checkLength([_]). %checkLength(X). comprueba junto con lengthEq que todas las listas tienen el mismo tamanio
checkLength([X,Y|T]):- lengthEq(X,Y),checkLength([Y|T]).

lengthEq(X,Y):-lengthEqCom(X,Y,0,0).% devuelve true si las dos listas tienen la misma longitud
lengthEqCom([],[],_,_).
lengthEqCom([_|B],[_|D],Z,P):- igual(Z,P),lengthEqCom(B,D,s(Z),s(P)).

append1([],Ys,[Ys]).     % append1(A,B,C) append1 crea una lista con  si no existe en C con B dentro, si existe , pone B a continuacion de A y devuelve en C
append1([X|Xs],Ys,[X|Zs]) :-append1(Xs,Ys,Zs).

suma(0, X, X):- nat(X). %suma(A,B,C), suma A + B devuelve en C
suma(s(X),Y,s(Z)):-suma(X,Y,Z).

resta(A, B, C) :- suma(C, B, A). % resta, usando el predicado de la suma a la inversa como C = A+B  sacamos la resta

array([[_]]). % comprueba que las listas del edificio sean listas de listas 
array([[_]|[_]]).
array([[_|_]|[_|_]]).

total_house([],T).  %cuenta el numero de viviendas en el edificio
total_house([X|P],T):- total_house(P,O),my_length(X,S),suma(O,S,T).
% Test total_ house total_house([[ s(0),s(s(s(0)))], [s(0),s(s(0))],[s(s(0)),s(s(0))],[0,s(s(0))]],T).

menorNr(0,s(X)) :- nat(X).% menorNr(A,B),comprueba si  A es menor que B
menorNr(s(X),s(Y)) :- menorNr(X,Y).

divide(D,Dd,C):-aux_divide(D,Dd,C,N). %divide(A,B,C), divide A entre B y devuelve la solucion truncada hacia abajo en C
aux_divide(D,Dd,C,N):-menorNr(D,Dd),igual(N,C);suma(N,s(0),M),resta(D,Dd,Z),aux_divide(Z,Dd,C,M).




levelN(X,N,C):-level_auxN(X,N,K),igual(C,K).  % no chekea que X sea building, es simplemente un buscador de lista N,
level_auxN([X|Y],N,C):-igual(N,s(0)),igual(C,X);(suma(Z,s(0),N),levelN(Y,Z,C)).

columnN(X,N,C):-column_auxN(X,N,K),naive_reverse(K,C).    % no chekea que X sea building, es simplemente un buscador de la N columna
column_auxN([],N,C).
column_auxN([X|Y],N,C):-column_auxN(Y,N,I),levelN(X,N,Cs),append1(I,Cs,C).





%------------------------  basic building type --------------------------------    funciona
basic_building(X):- array(X),basic_building1(X).%miro que no este vacio, para poder hacer despues la recursividad
basic_building1([]). % write_string("fin basic\n"),
basic_building1([X|T]) :- basic_building_comp(X,T). % write_string("iteracion piso\n"),
basic_building_comp([],K):-basic_building1(K).% write_string("fin piso, siguin\n"),
basic_building_comp([Z|C],K):-nat(Z),basic_building_comp(C,K). % write_string("iteracion, vivienda\n"),
%------------------------  basic building predicate tests ---------------------aaaaaaaws444
% basic_building([[s(0)],[0]]).yes
% basic_building([[s(0),0],[0,s(0)]]). yes
% basic_building([[s(0),s(s(s(0)))],[0,s(s(0))]]). yes
% basic_building([[0]]). yes
% basic_building([[0],0]). no
% basic_building([0]).no
% basic_building(0).no
% basic_building([[s(0)],s(0)]). no
% basic_building([s(0)]).no
% basic_building(s(0)).no
%------------------------- building type ----------------------------------  funciona
building(X):-basic_building(X),checkLength(X).
%------------------------ building predicate tests ---------------------------
% building([[0,s(0)],[0,0],[s(0),0],[0,0]]). yes
% building([[0,s(0)],[0,0],[s(0),0],[0]]).   no
%--------------------------  level predicate ---------------------------------------------------   funciona
level(X,N,C):- building(X),level1(X,N,A),igual(A,C).
level1(X,N,C):-level_aux(X,N,K),igual(C,K).
level_aux([X|Y],N,C):-igual(N,s(0)),igual(C,X);(suma(Z,s(0),N),level1(Y,Z,C)).
%--------------------------  test level predicate ---------------------------------------------------
% X=[[ s(0), s(s(s(0))) ], [ 0, s(s(0)) ] ],level(X, s(s(0)), C). C=[ 0, s(s(0)) ]
% level([[ s(0), s(s(s(0))) ], [ 0, s(s(0)) ] ], s(0), C).  C= [ s(0), s(s(s(0)))]
% level([[ s(0), s(s(s(0))) ], [ 0, s(s(0)),s(s(0))],[ s(0), s(0)]], s(s(s(0))), C). no
%-----------------------------------------------------------------------------------------------
%--------------------------  column predicate --------------------------------------------------- funciona
column(X,N,C):-building(X),column_aux(X,N,K),naive_reverse(K,C).
column_aux([],N,C).
column_aux([X|Y],N,C):-column_aux(Y,N,I),levelN(X,N,Cs),append1(I,Cs,C).
%--------------------------  test column predicate ---------------------------------------------------
% column([[ s(0),s(s(s(0)))], [s(0),s(s(0))],[s(s(0)),s(s(0))],[0,s(s(0))]],s(s(0)),C).
%-----------------------------------------------------------------------------------------------
%--------------------------- colums predicate ----------------------------------    funciona
columns(X,C):-building(X),level(X,s(0),F),my_length(F,P),columns_aux(X,0,P,K),naive_reverse(K,C).
columns_aux(X,N,N,C).
columns_aux(X,N,P,C):-suma(N,s(0),K),columns_aux(X,K,P,I),columnN(X,K,O),append1(I,O,C).
%--------------------------  test columns predicate ---------------------------------------------------
% columns([[ s(0),s(s(s(0)))],[s(0),s(s(0))]],C).
%-----------------------------------------------------------------------------------------------
%----------------------------------------------  total_people predicate --------------------------  funciona
total_people(X,T):-building(X),total_people1(X,A),igual(A,T).
total_people1([],T).
total_people1([X|P],T):- total_people1(P,O),total_people_rec(X,S),suma(O,S,T).
total_people_rec([],T).
total_people_rec([X|Y],T):-total_people_rec(Y,O),suma(O,X,T).

%--------------------------  test total_people predicate ---------------------------------------------------
% total_people([[ s(0),s(s(s(0)))], [s(0),s(s(0))],[s(s(0)),s(s(0))],[0,s(s(0))]],C).
%-----------------------------------------------------------------------------------------------
%-----------------------------------------------  average predicate -----------------
average(X,A):-building(X),total_people(X,J),total_house(X,K),divide(J,K,C),igual(A,C).
%--------------------------  test average predicate ---------------------------------------------------
%average([[ s(0),s(s(s(0)))], [s(0),s(s(0))],[s(s(0)),s(s(0))],[0,s(s(0))]],C).
%-----------------------------------------------------------------------------------
