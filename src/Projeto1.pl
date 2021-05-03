:- [codigo_comum].
:-[puzzles_publicos].

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
% zeros(L)
% true, se L for uma lista de zeros, false caso contrario
% --------------------------------------------------------------------------------
zeros([]).
zeros([H|T]):-
    H=:=0,
    zeros(T).

%---------------------------------------------------------------------------------
% get_varnum(F,L)
% Sendo F uma Fila,
% Significa que L e uma lista que contem as variaveis de um Espaco
% e o par com os numeros que vao determinar a soma do Espaco, dependendo
% se eh linha ou coluna
% --------------------------------------------------------------------------------
get_varnum(F,L):-
    get_varnum(F,L,0).


get_varnum([],[],2).

get_varnum([H|F],L,C):-
    C>=1,
    (var(H)->
        get_varnum(F,L2,2),
        append([H],L2,L);
    get_varnum([],L,C)).

get_varnum([H|F],L,0):-
    nonvar(H),
    \+zeros(H),
    get_varnum(F,L2,1),
    append([H],L2,L).

get_varnum([_|F],L,0):-
    get_varnum(F,L,0).

%********************************************************************************
% espaco_fila(Fila, Esp, H_V)
% Fila eh uma fila (linha ou coluna) de um puzzle e H_V eh um
% dos atomos h ou , conforme se trate de uma fila horizontal ou vertical,
% respetivamente.
% Esp eh um espaço de Fila, tal como descrito na Seccao 2.1, 
% passo 1, no enunciado
%********************************************************************************

espaco_fila(Fila, Esp , Hv):-
    get_varnum(Fila,Vnum),
    Vnum = [[E1,E2]|L],
    (Hv==h -> 
        Esp = espaco(E2,L);
    Esp = espaco(E1,L)).



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


% 3.1.5  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%Como definir as fincoes (nos comentarios) se adicionarmos um parametro?? 

%---------------------------------------------------------------------------------
% nonvars(L)
% true, se L for uma lista sem vars, false caso contrario
% --------------------------------------------------------------------------------
nonvars([]).
nonvars([H|T]):-
    nonvar(H),
    nonvars(T).

%********************************************************************************
% espacos_puzzle(Puzzle, Espacos)
% Puzzle eh a lista de espacos de Puzzle, tal como descrito na
% Seccao 2.1, passo 1 do enunciado.
%
% .......
%********************************************************************************

espacos_puzzle(P,E):-
    espacos_puzzle(P,E,h,P).

espacos_puzzle([],Esp,Hv,Pbase):-
    (Hv==h->
        mat_transposta(Pbase,Pnew),
        espacos_puzzle(Pnew,Esp,v,_);
    Esp=[]).

espacos_puzzle([P1|P2],Esp,Hv,Pbase):-
    %(maplist(nonvar,P1) -> isto dava aquele erro chato do __aux_maplist :(
    (nonvars(P1) -> 
        espacos_puzzle(P2,Esp,Hv,Pbase);
    espacos_fila(Hv,P1,NewP),
    espacos_puzzle(P2,Esp2,Hv,Pbase),
    append(NewP,Esp2,Esp)).
    

% 3.1.6  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% var_in_list(V,L2)
% V eh uma variavel
% Significa que a mesma variavel V encontra-se em L2
% --------------------------------------------------------------------------------
var_in_list(V,[H2|L2]):-
    (L2 == [] -> 
        V == H2;
    (V == H2 ->
        var_in_list(V,[H2]);
    var_in_list(V,L2))).
    
%---------------------------------------------------------------------------------
% vars_in_list(L1,L2)
% L1 e L2 sao uma lista de variaveis
% Significa que ha pelo menos uma variavel comum entre L1 e L2
% --------------------------------------------------------------------------------


vars_in_list([H1|_],L2):-
    var_in_list(H1,L2),
    !. %ja foi encontrada uma var igual

vars_in_list([_|L1],L2):-
    vars_in_list(L1,L2).



%********************************************************************************
% espacos_com_posicoes_comuns(Espacos,Esp,Esps_com)
% Espacoes eh uma lista de espacos e Esp eh um espaco.
% Significa que Esps_com eh a lista de espacos com variaveis em
% comum com Esp, exceptuando Esp.
% 
%********************************************************************************

espacos_com_posicoes_comuns([],_,[]).

espacos_com_posicoes_comuns([H|E],Esp,Ecom):-
    (H == Esp ->
        espacos_com_posicoes_comuns(E,Esp,Ecom);
    espaco(_,L1) = H,
    espaco(_,L2) = Esp,
    (vars_in_list(L1,L2)->
        espacos_com_posicoes_comuns(E,Esp,Ecom2),
        append([H],Ecom2,Ecom); %ponderar passar isto para [H1|Ecom2] ou nao
    espacos_com_posicoes_comuns(E,Esp,Ecom))).


% 3.1.7  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% permutacoes_soma_espacos(Espacos,Perms_soma)
% Espacos eh uma lista de espacos.
% Significa que Perms_soma eh a lista de listas de 2 elementos, em que
% o 1o elemento eh um espaco de Espacos e o 2o eh a lista ordenada
% de permutacoes cuja soma eh igual a soma do espaco
%********************************************************************************

permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([H1|E],P):-
    espaco(Soma,Vars) = H1,
    length(Vars,N),
    permutacoes_soma(N,[1,2,3,4,5,6,7,8,9],Soma,Perms),
    %H2 = [H1,Perms], Pq q nao estava a funcionar com isto?
    %writeln(H2),
    permutacoes_soma_espacos(E,P2),
    append([[H1,Perms]],P2,P).

% 3.1.8  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% espaco_get_perms_soma(Esp,Perms_soma,Eperm)
% Esp eh um Espaco e Perms_soma eh eh uma lista de listas tal como 
% obtido no predicado permutacoes_soma_espacos.
% Significa que Eperm eh a lista de permutacoes de Perm_soma associada
% ao espaco Esp.
% --------------------------------------------------------------------------------
espaco_get_perms_soma(E,[H|Psom],Eperm):-
    H = [Esp2,Perms],
    (Esp2 == E ->
        Eperm = Perms;
    espaco_get_perms_soma(E,Psom,Eperm)).

%---------------------------------------------------------------------------------
% muda_var(A,Var,L1,L2)
% A eh um atomo, Var uma variavel e L1 eh uma lista
% Significa que L2 eh a lista que resulta de substituir as variaveis Var 
% da lista L1 para A
% --------------------------------------------------------------------------------
muda_var(_,_,[],[]):-!.
muda_var(A,V,[H1|L1],Out):-
    (V==H1 ->
        muda_var(A,V,L1,Out2),
        append([A],Out2,Out);
    muda_var(A,V,L1,Out2),
    append([H1],Out2,Out)).

%---------------------------------------------------------------------------------
% muda_multi_var(La,Lvar,L1,L2)
% La eh uma lista de atomos, Lvar eh uma lista de variaveis e L1 eh uma lista
% O tamanho de La eh igual ao de Lvar
% Significa que L2 eh a lista  que resulta de substituir tds as 
% ocorrencias variaveis de Lvar pelo seu atomo correspondente em La.
% --------------------------------------------------------------------------------
muda_multi_var([H1|A],[H2|Var],L1,L2):-
    (A == [] ->
        muda_var(H1,H2,L1,L2);
    muda_var(H1,H2,L1,L3),
    muda_multi_var(A,Var,L3,L2)).



%---------------------------------------------------------------------------------
% permutacao_valida(Perm1,L1,Ecom,Psoma)
% L1 eh a lista com as variaveis a preencher por um dado espaco, Ecom 
% eh a lista de espacos comuns ao espaco com lista L1 e Psoma eh
% uma lista de listas tal como 
% obtido no predicado permutacoes_soma_espacos.
%
% Verifica se Perm1 eh uma permutacao valida para o espaco em estudo
% --------------------------------------------------------------------------------
permutacao_valida(Perm1,L1,[E1|Ecom],Psoma):-
    %se fizermos uma a uma nem vai ser preciso o mmvar2, apenas o mmvar1
    (Ecom==[]->
       true; 
    E1 = espaco(_,Lcom),
    muda_multi_var(Perm1,L1,Lcom,Lnew),
    espaco_get_perms_soma(E1,Psoma,PermCom),
    %verificar se unifica com pelo menos uma permutacao
    include(subsumes_term(Lnew),PermCom,Ucheck), 
    length(Ucheck,Size),
    Size>0,
    permutacao_valida(Perm1,L1,Ecom,Psoma)).




%********************************************************************************
% permutacao_possivel_espaco(Perm,Esp,Espacos,Perms_soma)
% Perm eh uma permutacao, Esp eh um espaco, Espacos eh uma
% lista de espacos e Perms_soma eh uma lista de listas tal como 
% obtido no predicado permutacoes_soma_espacos.
% Significa que Perm eh uma permutacao possivel para o espaco Esp,
% tal como descrito na Seccao 2.1, passo 2 do enunciado
%********************************************************************************

permutacao_possivel_espaco(P,E,Eos,Psoma):-
    espacos_com_posicoes_comuns(Eos,E,Ecom),
    espaco_get_perms_soma(E,Psoma,Eperm),
    permutacao_possivel_espaco(P,E,Eos,Psoma,Ecom,Eperm).

permutacao_possivel_espaco([],_,_,_,_,[]).

permutacao_possivel_espaco(P,E,Eos,Psoma,Ecom,[Perm1|Eperm]):-
    E = espaco(_,L1), %substituir isto no inicio (se der)
    writeln(' '),
    (permutacao_valida(Perm1,L1,Ecom,Psoma) -> 
        writeln(Perm1),
        P = Perm1;
    permutacao_possivel_espaco(P,E,Eos,Psoma,Ecom,Eperm)).
    


% 3.1.9  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% permutacoes_possiveis_espaco(Espacos, Psoma, Esp, Pposs)
% Esp eh um espaco, Espacos eh uma lista de espacos e 
% Psoma eh uma lista de listas tal como obtido 
% no predicado permutacoes_soma_espacos.
%
% Significa que Pposs eh é uma lista de 2 elementos em que o primeiro é a
% lista de variáveis de Esp e o segundo eh a lista ordenada de permutacoes 
% possiveis para o espaço Esp, tal como descrito na Secção 2.1,
% passo 2 do enunciado
%********************************************************************************

permutacoes_possiveis_espaco(Eos,Psoma,E,[EL|[PermL]]):-
    E = espaco(_,EL),
    espacos_com_posicoes_comuns(Eos,E,Ecom),
    espaco_get_perms_soma(E,Psoma,Eperm),
    permutacoes_possiveis_espaco(PermL,E,Eos,Psoma,Ecom,Eperm).

permutacoes_possiveis_espaco([],_,_,_,_,[]).

permutacoes_possiveis_espaco(Perm,E,Eos,Psoma,Ecom,[Perm1|Eperm]):-
    E = espaco(_,L1), %substituir isto no inicio (se der)
    (permutacao_valida(Perm1,L1,Ecom,Psoma)->
        permutacoes_possiveis_espaco(Perm2,E,Eos,Psoma,Ecom,Eperm),
        append([Perm1],Perm2,Perm);
    permutacoes_possiveis_espaco(Perm,E,Eos,Psoma,Ecom,Eperm)).