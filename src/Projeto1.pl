:- [codigo_comum].

% 3.1.1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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



%********************************************************************************
% combinacoes_soma(N, Els, Soma, Combs)
% N eh inteiro, Els eh uma lista de inteiros e Soma eh um inteiro.
% Significa que Combs eh a lista ordenada cujos elementos são as 
% combinacoes N a N, dos elementos de Els cuja soma eh Soma.
%
% Sao usados 2 acumuladores:
%  1. Para guardar as combinacoes ja encontradas
%  2. Guardar o numero de combinacoes repetidas durante o processo (o
% que controla se a funcao continua ou nao)
%********************************************************************************
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


% 3.1.2  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% insere(L1,L2,L3)
% L1 eh uma lista de inteiros, L2 e L3 sao listas de listas de inteiros
% L3 resulta de adicionar L1 a L2, de modo a que fique ordenado segundo 
% o criterio do smaller.
% --------------------------------------------------------------------------------

insere(El1,[],[El1]):-!. %Se chegar aqui, nao queremos mais solucoes

insere(El1,[H1|T1],[H2|T2]):-
    smaller(El1,H1)->
        H2=El1,
        T2=[H1|T1];
    insere(El1,T1,T2),
    H2 = H1. 

%---------------------------------------------------------------------------------
% insere_multi(L1,L2,L3)
% L1, L2 e L3 sao listas de listas de inteiros
% L3 resulta de de fazer insere de todos as sublistas da lista L1 em L2
% --------------------------------------------------------------------------------

insere_multi([],L2,L2).

insere_multi([H1|T1],L2,L3):-
    insere_multi(T1,L2,L4),
    insere(H1,L4,L3).
%---------------------------------------------------------------------------------
% smaller(L1,L2)
% L1 eh uma lista de inteiros, tal como L2 (ambas com o mesmo tamanho)
% Verifica se o numero formado ao juntar os numeros de L1 eh menor
% que o de L2.
% --------------------------------------------------------------------------------
smaller([],[]).
smaller([H1|T1],[H2|T2]):-
    H1 < H2 ->
        smaller([],[]);
    H1 =:= H2 ->
        smaller(T1,T2).


%********************************************************************************
% permutacoes_soma(N, Els, Soma, Combs)
% N eh inteiro, Els eh uma lista de inteiros e Soma eh um inteiro.
% Significa que Perms eh a lista ordenada cujos elementos são as 
% permutacoes das combinacoes N a N, dos elementos de Els cuja soma eh Soma.
%
% Sao usados 2 acumuladores:
%  1. Para guardar as combinacoes ja encontradas
%  2. Guardar o numero de combinacoes repetidas durante o processo (o
% que controla se a funcao continua ou nao)
%********************************************************************************
permutacoes_soma(N,E,S,C):-
    permutacoes_soma(N,E,S,C,[],0).

permutacoes_soma(_,[],_,[[]],_,_):-!.
permutacoes_soma(0,_,_,[[]],_,_):-!.
permutacoes_soma(_,_,0,[[]],_,_):-!.
permutacoes_soma(-1,_,_,[],_,_):-!.

%Por fazer
permutacoes_soma(Num,Els,Soma,Comb1,Acc1,Acc2):-
    combinacao(Num,Els,NewC),
    somatorio(NewC,Soma),
    \+member(NewC,Acc1) ->
        findall(P,permutation(NewC,P),NewP),
        permutacoes_soma(Num,Els,Soma,Comb2,[NewC|Acc1],0),
        insere_multi(NewP,Comb2,Comb1);

    length(Acc1,Size),
    Acc2 =:= Size ->
        permutacoes_soma(-1,Els,Soma,Comb1,Acc1,Acc2);
    
    Acc2_1 is Acc2+1,
    permutacoes_soma(Num,Els,Soma,Comb1,Acc1,Acc2_1).






% 3.1.3  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


%get_numberX(L,N) numeros do espaco


get_numberX([[0,0]|_],0).
get_numberX([],0).

get_numberX([H1|T1],N):-
    length(H1,Size),!, %nao queremos mais Size (assim, se for _, Size <- 0)
    Size =\= 0->
        [E1,_] = H1,
        get_numberX(T1,N_new),
        N is N_new+E1,!;
    get_numberX(T1,N).





/*
espaco_num_list([],_,0,[]).

espaco_num_list([H1|T1],Hv,N,L):-
    length(H1,Size),!, %nao queremos mais Size (assim, se for _, Size <- 0)
    Size =\= 0 -> 
        [El1,El2] = H1,
        El1 =\= 0 -> 
            Hv==h -> 
                espaco_num_list(T1,Hv,El1,L);   %linhas
            espaco_num_list(T1,Hv,El2,L);       %colunas
        ;
    espaco_num_list(T1,Hv,N,L2),
    junta([H1],L2,L).
*/


%---------------------------------------------------------------------------------
% espaco_fila(Fila, Esp, H_V)
% Fila eh uma fila (linha ou coluna) de um puzzle e H_V eh um
% dos atomos h ou , conforme se trate de uma fila horizontal ou vertical,
% respetivamente.
% Esp eh um espaço de Fila, tal como descrito na Seccao 2.1, 
% passo 1, no enunciado
%---------------------------------------------------------------------------------











