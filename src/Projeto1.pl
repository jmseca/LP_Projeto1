:- [codigo_comum].

% 3.1.1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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
    sum_list(NewC,Soma),
    \+member(NewC,Acc1) ->
        combinacoes_soma(Num,Els,Soma,Comb2,[NewC|Acc1],0),
        append([NewC],Comb2,Comb1);

    length(Acc1,Size),
    Acc2 =:= Size ->
        combinacoes_soma(-1,Els,Soma,Comb1,Acc1,Acc2);
    
    Acc2_1 is Acc2+1,
    combinacoes_soma(Num,Els,Soma,Comb1,Acc1,Acc2_1).


% 3.1.2  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

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

%---------------------------------------------------------------------------------
% insere(L1,L2,L3)
% L1 eh uma lista de inteiros, L2 e L3 sao listas de listas de inteiros
% L3 resulta de adicionar L1 a L2, de modo a que fique ordenado segundo 
% o criterio da funcao smaller.
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

permutacoes_soma(Num,Els,Soma,Comb1,Acc1,Acc2):-
    combinacao(Num,Els,NewC),
    sum_list(NewC,Soma),
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

%---------------------------------------------------------------------------------
% aux_313(Fila,L,N,Hv)
% Funcao auxiliar do espaco_fila.
% Fila eh uma fila (linha ou coluna) de um puzzle e H_V eh um
% dos atomos h ou , conforme se trate de uma fila horizontal ou vertical,
% respetivamente.
%
% Significa que L e N sao a lista de posicoes e a soma dessas posicoes, 
% de um espaco, respetivamente
%
% Sao usados 3 acumuladores
%   1. Guarda as variaveis indefinidas
%   2. Serve para controlar que ramo da funcao devemos seguir
%   3. Guarda o numero que sera do N
% --------------------------------------------------------------------------------


get_X(F,L,N,Hv):-
    get_X(F,L,N,Hv,[],0,0).

get_X(_,L,Num,_,L,1,Num).


get_X([H1|R],L,N,Hv,Acc,_,AccNum):-
    is_list(R),
    R == [],
    (var(H1)->
        append(Acc,[H1],Acc3);
    Acc3 = Acc),
    get_X(R,L,N,Hv,Acc3,1,AccNum).


get_X([H1|T1],L,N,Hv,Acc1,_,AccNum):-
    (nonvar(H1) ->
        H1 = [E1,E2],
        (maplist(=:=(0),[E1,E2]) ->
            get_X(T1,L,N,Hv,Acc1,1,AccNum);
            (Hv == h ->
                get_X(T1,L,N,Hv,[],0,E2);
            Hv == v,
            get_X(T1,L,N,Hv,[],0,E1))
            )
        ;
    append(Acc1,[H1],Acc3),
    get_X(T1,L,N,Hv,Acc3,0,AccNum)).

%********************************************************************************
% espaco_fila(Fila, Esp, H_V)
% Fila eh uma fila (linha ou coluna) de um puzzle e H_V eh um
% dos atomos h ou , conforme se trate de uma fila horizontal ou vertical,
% respetivamente.
% Esp eh um espaço de Fila, tal como descrito na Seccao 2.1, 
% passo 1, no enunciado
%********************************************************************************

espaco_fila(Fila, Esp , Hv):-
    get_X(Fila,L,N,Hv),
    Esp = espaco(N,L).



% 3.1.4  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% espacos_fila(Hv, Fila, Espacos)
% Fila eh uma fila (linha ou coluna) de uma grelha e Hv eh 
% um dos atomos h ou v. 
% Significa que Espacos eh a lista de todos os espacos de Fila, 
% da esquerda para a direita 
%********************************************************************************

espacos_fila(Hv,F,Esp):-
    bagof(X,espaco_fila(F,X,Hv),Esp).
    









