%-------------------------------------------------------------------------------
%                combinacao(N, Els, Comb)
% combinacao(N, Els, Comb) significa que Comb eh uma
% combinacao dos elementos de Els, N a N
%-------------------------------------------------------------------------------
combinacao(0,_,[]).
combinacao(N,[X|T], [X|Comb]):-
    N > 0,
    N1 is N-1,
    combinacao(N1, T, Comb).
combinacao(N, [_|T], Comb):-
    N > 0,
    combinacao(N, T, Comb).

%---------- Para cima são códigos comuns

% 3.1.1 

%--------------------------------------------------------------------------------
% somatorio(L,N)
% L eh uma lista de inteiro.
% somatorio(L,N) significa que N eh um inteiro que representa
% eh a soma dos elementos da lista L.
%--------------------------------------------------------------------------------

somatorio([],0).
somatorio([H|T],N) :-
    somatorio(T,N_new),
    N is +(N_new,H).


%--------------------------------------------------------------------------------
% junta(L1,L2,L3)
% L1, L2 e L3 sao duas listas.
% L3 eh o resultado de juntar a lista L1 e a lista L2
%--------------------------------------------------------------------------------
junta([],L2,L2).

junta([H1|T1],L2,[H1|T3]):-
    junta(T1,L2,T3).


%--------------------------------------------------------------------------------
% inverte(L1,L2)
% L1 e L2 sao duas listas.
% Cada uma eh o inverso da outra
%--------------------------------------------------------------------------------
inverte([],[]).

inverte([H1|T1],L2):-
    inverte(T1,L3),
    junta(L3,[H1],L2).



%--------------------------------------------------------------------------------
% combinacoes_soma(N, Els, Soma, Combs)
% N eh inteiro, Els eh uma lista de inteiros e Soma eh um inteiro.
% Significa que Combs eh a lista ordenada cujos elementos são as 
% combinacoes N a N, dos elementos de Els cuja soma eh Soma.
%
% Sao usados 2 acumuladores:
%  1. Para guardar as combinacoes ja encontradas
%  2. Guardar o numero de combinacoes repetidas durante o processo (o
% que controla se a funcao continua ou nao)
%--------------------------------------------------------------------------------
combinacoes_soma(N,E,S,C):-
    combinacoes_soma(N,E,S,C,[],0).

%Pq e q tenho de colocar isto aqui? (:-!.)
%Nos outros programas ele nao devolve logo a afirmacao 
%exemplo: funcos mais simples.
combinacoes_soma(_,[],_,[[]],_,_):-!.
combinacoes_soma(0,_,_,[[]],_,_):-!.
combinacoes_soma(_,_,0,[[]],_,_):-!.
combinacoes_soma(-1,_,_,[],_,_):-!.

combinacoes_soma(Num,Els,Soma,Comb1,Acc1,Acc2):-
    writeln(gone_here),
    combinacao(Num,Els,NewC),
    somatorio(NewC,Soma),
    \+member(NewC,Acc1) ->
        combinacoes_soma(Num,Els,Soma,Comb2,[NewC|Acc1],0),
        junta([NewC],Comb2,Comb1);

    length(Acc1,Size),
    Acc2 =:= Size ->
        combinacoes_soma(-1,Els,Soma,Comb1,Acc1,Acc2);
    
    Acc2_1 is Acc2+1,
    combinacoes_soma(Num,Els,Soma,Comb1,Acc1,Acc2_1).


    





