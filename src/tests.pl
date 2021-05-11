
:- [codigo_comum].


sum_equal(N,L):-
    sum_list(L,Size),
    Size =:= N.

%********************************************************************************
% combinacoes_soma(N, Els, Soma, Combs)
% N eh inteiro, Els eh uma lista de inteiros e Soma eh um inteiro.
% Significa que Combs eh a lista ordenada cujos elementos sao as 
% combinacoes N a N, dos elementos de Els cuja soma eh Soma.
%
% Sao usados 2 acumuladores:
%  1. Para guardar as combinacoes ja encontradas
%  2. Guardar o numero de combinacoes repetidas durante o processo (o
% que controla se a funcao continua ou nao)
%********************************************************************************
combinacoes_somaz(N,Els,Soma,Combs):-
    findall(X,combinacao(N,Els,X),PreCombs),
    include(sum_equal(Soma),PreCombs,Combs).

%combinacoes_somaz(N,Els,Soma,[C1|Comb1],Acc):-
%    combinacao(N,Els,C1),
%    sum_list(C1,Soma),
%    \+member(C1,Acc),
%    writeln(C1),
%    combinacoes_somaz(N,Els,Soma,Comb1,[C1|Acc]).