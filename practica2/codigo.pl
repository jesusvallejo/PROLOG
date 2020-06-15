:- module(_,_,[assertions]).
:- use_module(library(unittest)).
:- use_module(library(aggregates)).
:- use_module(library(lists)).
:- use_module(engine(basic_props)).
 alumno_prode('Vallejo','Collados','Jesus','X150319').
 alumno_prode('Rubio','Martin','Roberto','w140035').
 alumno_prode('Montanez','Soria','Diego','X150336').


%% ---------------------------- eliminar_comodines ----------------------------------------
eliminar_comodines(Registros,RegistrosSinComodines, ListaSimbolos) :-
	functor(Registros,regs,N),
	N>1,
	functor(RegistrosSinComodines,regs,N),
	Registros =.. Parametros,
	headOff(Parametros,SinCabeza),
	eliminar_comodines_aux(SinCabeza,1,RegistrosSinComodines),
	getList(Registros,ListaSimbolos),
	!.

eliminar_comodines_aux([],Cont,Sol).
eliminar_comodines_aux([A|B],Cont,Sol) :-
	(A \= *,
	 arg(Cont,Sol,A),
	 Cont1 is Cont + 1,
	 eliminar_comodines_aux(B,Cont1,Sol));
	(Cont1 is Cont + 1,
	 eliminar_comodines_aux(B,Cont1,Sol)).

getList(Registros,Lista) :-
	Registros =.. X,
	headOff(X,K),
	my_delete(K,*,Lista).
headOff([_|Tail],Tail).

my_delete([],X,[]).
my_delete([X|Xs],X,Ys) :-
	my_delete(Xs,X,Ys).
my_delete([X|Xs],Z,[X|Ys]) :-
	X \= Z,
	my_delete(Xs,Z,Ys).



%% eliminar_comodines(regs(1,2,*),B,A).

%% -----------------------------------------------------------------------------------------------


%% --------------------------- ejecutar_instruccion ----------------------------------------------

ejecutar_instruccion(EstadoActual, Instruccion, EstadoSiguiente) :-
	functor(EstadoActual,regs,N),
	N>1,
	functor(Instruccion,swap,2),
	functor(EstadoSiguiente,regs,N),
	my_swap(EstadoActual,Instruccion,EstadoSiguiente,1,Aux),
	arg(1, Aux,I),
	arg(2, Aux,J),
	nonvar(I),
	nonvar(J),
	N>=I,
	N>=J,
	I\=0,
	J\=0,
	I<J,
	I\=J,
	EstadoActual\=EstadoSiguiente.
																				


my_swap(EstadoActual, Instruccion,EstadoSiguiente,Cont,Aux) :-
	arg(1,Instruccion,PrimeraPos),
	arg(2,Instruccion,SegundaPos),
	functor(EstadoActual,N,Tam),
	Tam>=Cont,
	igual(Cont,PrimeraPos),
	arg(Cont,EstadoActual,Segundo),
	hazteUnFavor(EstadoSiguiente,SegundaPos,Segundo,1),
	Cont1 is Cont + 1,
	my_swap(EstadoActual, Instruccion,EstadoSiguiente,Cont1,Aux). 

my_swap(EstadoActual,Instruccion,EstadoSiguiente,Cont,Aux) :-
        arg(1,Instruccion,PrimeraPos),
	arg(2,Instruccion,SegundaPos),
	functor(EstadoActual,N,Tam),
	Tam>=Cont,
	igual(Cont,SegundaPos),
	arg(Cont,EstadoActual,Primero),
	hazteUnFavor(EstadoSiguiente,PrimeraPos,Primero,1),
	Cont2 is Cont + 1,
	my_swap(EstadoActual, Instruccion,EstadoSiguiente,Cont2,Aux). 

my_swap(EstadoActual,Instruccion,EstadoSiguiente,Cont,Aux) :-
	functor(EstadoActual,N,Tam),
	Tam>=Cont,
	arg(Cont,EstadoActual,K),
	arg(Cont,EstadoSiguiente,K),
	Cont3 is Cont + 1,
	my_swap(EstadoActual, Instruccion,EstadoSiguiente,Cont3,Aux).    

my_swap(EstadoActual,Instruccion,EstadoSiguiente,Cont,Aux) :-
	functor(EstadoActual,N,Tam),
	Cont>Tam,
	igual(Aux,Instruccion).


hazteUnFavor(EstadoActual,Posicion,Valor,Cont) :-
	functor(EstadoActual,N,Tam),
	Tam>=Cont,
	arg(Cont,EstadoActual,K),
	igual(Valor,K),
	Posicion is Cont.
hazteUnFavor(EstadoActual,Posicion,Valor,Cont) :-
	Cont1 is Cont +1,
	functor(EstadoActual,N,Tam),
	Tam>=Cont,
	hazteUnFavor(EstadoActual,Posicion,Valor,Cont1).


%%hazteUnFavor(regs(14,145,28),Posicion,28,1).
%%hazteUnFavor(regs(14,145,28),3,Valor,1).
%%hazteUnFavor(regs(14,145,28),28,X,1).

ejecutar_instruccion(EstadoActual, Instruccion, EstadoSiguiente) :-
	functor(EstadoActual,regs,N),
	N>1,
	functor(Instruccion,move,1),
	functor(EstadoSiguiente,regs,N),
	arg(1, Instruccion,I),
	my_move(EstadoActual,Instruccion,EstadoSiguiente,1),
	nonvar(I),
	N>=I,
	I\=0,
	EstadoActual\=EstadoSiguiente.
																					
my_move(EstadoActual,Instruccion,EstadoSiguiente,Cont) :-
	arg(1,Instruccion,Pos),
	Cont2  is Cont - 1 ,
	igual(Cont2,Pos),
	functor(EstadoActual,N,Tam),
	Tam>=Cont,
	arg(Pos,EstadoActual,K),
	arg(Cont,EstadoSiguiente,K),
	Cont1 is Cont +1,
	my_move(EstadoActual,Instruccion,EstadoSiguiente,Cont1).

my_move(EstadoActual,Instruccion,EstadoSiguiente,Cont) :-
	arg(1,Instruccion,Pos),
	functor(EstadoActual,N,Tam),
	Tam>=Cont,
	igual(Pos,Tam),
	Pos\=0,
	igual(Cont,1),
	arg(Pos,EstadoActual,K),
	arg(Cont,EstadoSiguiente,K),
	Cont1 is Cont +1,
	my_move(EstadoActual,Instruccion,EstadoSiguiente,Cont1).

my_move(EstadoActual,Instruccion,EstadoSiguiente,Cont) :-
	arg(Cont,EstadoActual,K),
	arg(Cont,EstadoSiguiente,K),
	Cont1 is Cont +1,
	functor(EstadoActual,N,Tam),
	Tam>=Cont,
	my_move(EstadoActual,Instruccion,EstadoSiguiente,Cont1).

my_move(EstadoActual,Instruccion,EstadoSiguiente,Cont) :-
	functor(EstadoActual,N,Tam),
	Cont >Tam.

igual(X,X).

%% ejecutar_instruccion(regs(1,2,3),move(1),regs(1,2,3)).
%% ejecutar_instruccion(regs(1,2,3,4,5),move(3),X).
%% ejecutar_instruccion(regs(1,2,3),swap(1,3),X).
%% ejecutar_instruccion(regs(1,2,3),X,regs(1,1,_)).
%% ejecutar_instruccion(regs(1,2,3),X,regs(2,1,3)).

%%------------------------------------------------------------------------------------------------------------------------
%% --------------------------- generador_de_codigo -----------------------------------------------------------------------


generador_de_codigo(EstadoInicial, EstadoFinal, ListaDeInstrucciones) :- eliminar_comodines(EstadoInicial,K, ListaSimbolos),
	generador_de_codigo_aux(K, EstadoFinal,ListaDeInstrucciones,[]).

generador_de_codigo(EstadoFinal,EstadoFinal,ListaDeInstrucciones).






generador_de_codigo_aux(EstadoInicial, EstadoFinal,[A|B],Estados_Visitados) :-
	\+ member(A,Estados_Visitados),append2(Estados_Visitados,A,K),ejecutar_instruccion(EstadoInicial,A,H),
	generador_de_codigo_aux(H,EstadoFinal,B,K).
generador_de_codigo_aux(EstadoFinal, EstadoFinal,ListaDeInstrucciones,Estados_Visitados).

append2( [], X, [X]).                                   %%appends into list or creates a new list if list is empty
append2( [X | Y], Z, [X | W]) :- append( Y, Z, W).
%%------------------------------------------------------------------------------------------------------------------------
%% generador_de_codigo(regs(1,2,3,4), EstadoFinal,[swap(1,2),move(2)]).
%% generador_de_codigo(regs(1,2,3),regs(2,1,3),X).
%% generador_de_codigo(regs(1,2,3),regs(2,3,1),X).



