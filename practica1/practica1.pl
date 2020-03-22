
:-use_module(library(stream_utils)).

nat(0).               
nat(s(X)) :- nat(X).
%------------------------  basic building predicate --------------------------------
basic_building([]):-!. % write_string("fin basic\n"),
basic_building([X|T]) :- basic_building_comp(X,T). % write_string("no ultima lista\n"),
basic_building_comp([],K):-basic_building(K).% write_string("fin comp\n"),
basic_building_comp([Z|C],K):-nat(Z),basic_building_comp(C,K). % write_string("iteracion, no ultima lista\n"),
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
%-------------------------------------------------------------------------------------