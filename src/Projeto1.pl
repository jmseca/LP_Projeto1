% Joao Fonseca 95749

:- [codigo_comum].

% 3.1.1 <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% sum_equal(N,L)
% N eh um inteiro e L uma lista.
%
% Devolve true se N for a soma de todos os elementos da lista L, 
% false caso contrario
% --------------------------------------------------------------------------------

sum_equal(N,L):-
    sum_list(L,Size),
    Size =:= N.

%********************************************************************************
% combinacoes_soma(N, Els, Soma, Combs)
% N eh inteiro, Els eh uma lista de inteiros e Soma eh um inteiro.
%
% Significa que Combs eh a lista ordenada cujos elementos sao as 
% combinacoes N a N, dos elementos de Els cuja soma eh Soma.
%********************************************************************************

combinacoes_soma(N,Els,Soma,Combs):-
    findall(X,combinacao(N,Els,X),PreCombs),
    include(sum_equal(Soma),PreCombs,Combs).

% 3.1.2  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% findall_permutacoes(L1,L2)
% L1 eh uma lista.
% 
% Significa que L2 eh a lista com todas as permutacoes de L1
% --------------------------------------------------------------------------------
findall_permutacoes(L1,L2):-
    findall(X,permutation(L1,X),L2).


%********************************************************************************
% permutacoes_soma(N, Els, Soma, Combs)
% N eh inteiro, Els eh uma lista de inteiros e Soma eh um inteiro.
%
% Significa que Perms eh a lista ordenada cujos elementos sao as 
% permutacoes das combinacoes N a N, dos elementos de Els cuja soma eh Soma.
%********************************************************************************
permutacoes_soma(N,Els,Soma,Combs):-
    combinacoes_soma(N,Els,Soma,PreComb1),
    maplist(findall_permutacoes,PreComb1,PreComb2),
    append(PreComb2,PreComb3),
    sort(PreComb3,Combs).


% 3.1.3  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% get_varnum(F,L)
% F eh uma Fila.
%
% Significa que L e uma lista que contem as variaveis de um Espaco
% e o par com os numeros que vao determinar a soma do Espaco, dependendo
% se eh linha ou coluna.
% 
% Este predicado e dps definido tambem com aridade 3
% get_varnum(F,L,C)
% O novo argumento servira para guardar o estado (0,1 ou 2), onde:
% 0 -> o predicado apenas esta a percorrer a lista para encontrar um espaco
% 1 -> um espaco foi encontrado
% 2 -> foi encontrado o fim do espaco em analise
% --------------------------------------------------------------------------------
get_varnum(F,L):-
    get_varnum(F,L,0).

get_varnum([],[],2).

get_varnum([H|F],L,C):-
    C>=1,
    (var(H)->
        % foi encontrado o fim de um espaco
        get_varnum(F,L2,2),
        append([H],L2,L);
    get_varnum([],L,C)).

get_varnum([H|F],L,0):-
    nonvar(H),
    \+maplist(=:=(0),H),
    % foi encontrado o inicio de um espaco
    get_varnum(F,L2,1),
    append([H],L2,L).

% continuar a procura de espacos
get_varnum([_|F],L,0):-
    get_varnum(F,L,0).

%********************************************************************************
% espaco_fila(Fila, Esp, H_V)
% Fila eh uma fila (linha ou coluna) de um puzzle e H_V eh um
% dos atomos h ou , conforme se trate de uma fila horizontal ou vertical,
% respetivamente.
%
% Significa que Esp eh um espaco de Fila, tal como descrito na Seccao 2.1, 
% passo 1, no enunciado
%********************************************************************************

espaco_fila(Fila, Esp , H_V):-
    get_varnum(Fila,Vnum),
    Vnum = [[E1,E2]|L],
    (H_V==h -> 
        Esp = espaco(E2,L);
    Esp = espaco(E1,L)).



% 3.1.4  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% espacos_fila(Hv, Fila, Espacos)
% Fila eh uma fila (linha ou coluna) de uma grelha e Hv eh 
% um dos atomos h ou v. 
%
% Significa que Espacos eh a lista de todos os espacos de Fila, 
% da esquerda para a direita 
%********************************************************************************

espacos_fila(Hv,Fila,Espacos):-
    bagof(X,espaco_fila(Fila,X,Hv),Espacos).


% 3.1.5  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% espacos_puzzle(Puzzle, Espacos)
% Puzzle eh um puzzle.
%
% Significa que Espacos eh a lista de espacos de Puzzle, tal como descrito na
% Seccao 2.1, passo 1 do enunciado.
%
% Este predicado e dps definido tambem com aridade 4 
% espacos_puzzle(Puzzle, Espacos, Hv, Pbase)
% Hv -> indica se estamos a encontrar os espacos linha ou coluna
% Pbase -> Puzzle recebido como argumento inicial, servira para 
% depois se fazer a transposta do Puzzle sem problemas.
%********************************************************************************

espacos_puzzle(Puzzle,Espacos):-
    espacos_puzzle(Puzzle,Espacos,h,Puzzle).

espacos_puzzle([],Esp,Hv,Pbase):-
    (Hv==h->
        mat_transposta(Pbase,Pnew),
        espacos_puzzle(Pnew,Esp,v,_);
    Esp=[]).

espacos_puzzle([P1|Puzzle],Esp,Hv,Pbase):-
    (maplist(nonvar,P1) -> 
        espacos_puzzle(Puzzle,Esp,Hv,Pbase);
    espacos_fila(Hv,P1,NewP),
    espacos_puzzle(Puzzle,Esp2,Hv,Pbase),
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
% L1 e L2 sao listas de variaveis
% Significa que ha pelo menos uma variavel comum entre L1 e L2
% --------------------------------------------------------------------------------


vars_in_list([H1|_],L2):-
    var_in_list(H1,L2),!. %ja foi encontrada uma var igual

vars_in_list([_|L1],L2):-
    vars_in_list(L1,L2).



%********************************************************************************
% espacos_com_posicoes_comuns(Espacos,Esp,Ecom)
% Espacoes eh uma lista de espacos e Esp eh um espaco.
% Significa que Ecom eh a lista de espacos com variaveis em
% comum com Esp, exceptuando Esp.
% 
%********************************************************************************

espacos_com_posicoes_comuns([],_,[]).

espacos_com_posicoes_comuns([H|Espacos],Esp,Ecom):-
    (H == Esp ->
        espacos_com_posicoes_comuns(Espacos,Esp,Ecom);
    espaco(_,L1) = H,
    espaco(_,L2) = Esp,
    (vars_in_list(L1,L2)->
        espacos_com_posicoes_comuns(Espacos,Esp,Ecom2),
        Ecom = [H|Ecom2];
    espacos_com_posicoes_comuns(Espacos,Esp,Ecom))).


% 3.1.7  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% permutacoes_soma_espacos(Espacos,Psoma)
% Espacos eh uma lista de espacos.
% Significa que Psoma eh a lista de listas de 2 elementos, em que
% o 1o elemento eh um espaco de Espacos e o 2o eh a lista ordenada
% de permutacoes cuja soma eh igual a soma do espaco
%********************************************************************************

permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([H1|Espacos],Psoma):-
    espaco(Soma,Vars) = H1,
    length(Vars,N),
    permutacoes_soma(N,[1,2,3,4,5,6,7,8,9],Soma,Perms),
    permutacoes_soma_espacos(Espacos,P2),
    append([[H1,Perms]],P2,Psoma).

% 3.1.8  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% espaco_get_perms_soma(Esp,Psoma,Eperm)
% Esp eh um Espaco e Perms_soma eh uma lista de listas tal como 
% obtido no predicado permutacoes_soma_espacos.
% Significa que Eperm eh a lista de permutacoes de Perm_soma associada
% ao espaco Esp.
% --------------------------------------------------------------------------------
espaco_get_perms_soma(Esp,[H|Psoma],Eperm):-
    H = [Esp2,Perms],
    (Esp2 == Esp ->
        Eperm = Perms;
    espaco_get_perms_soma(Esp,Psoma,Eperm)).

%---------------------------------------------------------------------------------
% muda_var(A,Var,L1,L2)
% A eh uma constante, Var uma variavel e L1 eh uma lista
%
% Significa que L2 eh a lista que resulta de substituir as variaveis Var 
% da lista L1 para A
% --------------------------------------------------------------------------------
muda_var(_,_,[],[]).
muda_var(A,Var,[H1|L1],L2):-
    (Var==H1 ->
        muda_var(A,Var,L1,PreL2),
        append([A],PreL2,L2);
    muda_var(A,Var,L1,PreL2),
    append([H1],PreL2,L2)).

%---------------------------------------------------------------------------------
% muda_multi_var(La,Lvar,L1,L2)
% La eh uma lista de constantes, Lvar eh uma lista de variaveis e L1 eh uma lista
% O tamanho de La eh igual ao de Lvar
%
% Significa que L2 eh a lista  que resulta de substituir tds as 
% ocorrencias das variaveis de Lvar em L1 pela constante correspondente em La.
% --------------------------------------------------------------------------------

muda_multi_var([H1|La],[H2|Lvar],L1,L2):-
    (La == [] ->
        muda_var(H1,H2,L1,L2);
    muda_var(H1,H2,L1,L3),
    muda_multi_var(La,Lvar,L3,L2)).



%---------------------------------------------------------------------------------
% permutacao_valida(Perm1,L1,Ecom,Psoma)
% L1 eh a lista com as variaveis a preencher por um dado espaco, Ecom 
% eh a lista de espacos comuns ao espaco com lista L1 e Psoma eh
% uma lista de listas tal como 
% obtido no predicado permutacoes_soma_espacos.
%
% Verifica se Perm1 eh uma permutacao valida para o espaco em estudo
% --------------------------------------------------------------------------------

permutacao_valida(_,_,[],_).

permutacao_valida(Perm1,L1,[E1|Ecom],Psoma):-
    E1 = espaco(_,Lcom),
    muda_multi_var(Perm1,L1,Lcom,Lnew),
    espaco_get_perms_soma(E1,Psoma,PermCom),
    include(subsumes_term(Lnew),PermCom,Ucheck), 
    length(Ucheck,Size),
    Size>0,
    permutacao_valida(Perm1,L1,Ecom,Psoma).




%********************************************************************************
% permutacao_possivel_espaco(Perm,Esp,Espacos,Psoma)
% Perm eh uma permutacao, Esp eh um espaco, Espacos eh uma
% lista de espacos e Psoma eh uma lista de listas tal como 
% obtido no predicado permutacoes_soma_espacos.
%
% Significa que Perm eh uma permutacao possivel para o espaco Esp,
% tal como descrito na Seccao 2.1, passo 2 do enunciado
%********************************************************************************

permutacao_possivel_espaco(Perm,Esp,Espacos,Psoma):-
    espacos_com_posicoes_comuns(Espacos,Esp,Ecom),
    espaco_get_perms_soma(Esp,Psoma,Eperm),
    Esp = espaco(_,L1), 
    member(Perm,Eperm),
    permutacao_valida(Perm,L1,Ecom,Psoma).
    


% 3.1.9  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% permutacoes_possiveis_espaco(Espacos, Psoma, Esp, Pposs)
% Esp eh um espaco, Espacos eh uma lista de espacos e 
% Psoma eh uma lista de listas tal como obtido 
% no predicado permutacoes_soma_espacos.
%
% Significa que Pposs eh eh uma lista de 2 elementos em que o primeiro eh a
% lista de variaveis de Esp e o segundo eh a lista ordenada de permutacoes 
% possiveis para o espaco Esp, tal como descrito na Seccao 2.1,
% passo 2 do enunciado
%********************************************************************************

permutacoes_possiveis_espaco(Espacos,Psoma,Esp,Pposs):-
    Esp = espaco(_,ELst),
    findall(X,permutacao_possivel_espaco(X,Esp,Espacos,Psoma),PermL),
    Pposs = [ELst,PermL].

% 3.1.10  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% Espacos eh uma lista de espacos
%
% Significa que Perms_poss_esps eh a lista de permutacoes possiveis,
% tal como descrito na Seccao 2.1, no passo 2.
%********************************************************************************

permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
    permutacoes_soma_espacos(Espacos,Psoma),
    maplist(permutacoes_possiveis_espaco(Espacos, Psoma),Espacos,Perms_poss_esps).
    

% 3.1.11  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
%---------------------------------------------------------------------------------
% elimina_primeiros_el(L,L1)
% L eh uma lista de listas
% Significa que L1 eh L sem o primeiro elemento de cada sublista.
% --------------------------------------------------------------------------------

elimina_primeiros_el([],[]).

elimina_primeiros_el([[_|T1]|L],[T1|L1]):-
    elimina_primeiros_el(L,L1).

%---------------------------------------------------------------------------------
% primeiro_el_igual(El1,L)
% L eh uma lista de listas
% Significa que El1 eh o primeiro elemento de todas as sublistas de L.
% --------------------------------------------------------------------------------

primeiro_el_igual(_,[]).

primeiro_el_igual(El1,[[El1|_]|L]):-
    primeiro_el_igual(El1,L).


%********************************************************************************
% numeros_comuns(Lst_Perms, Numeros_comuns)
% Lst_Perms eh uma lista de permutacoes
%
% Significa que Numeros_comuns eh uma lista de pares (pos, numero),
% significando que todas as listas de Lst_Perms contem o numero numero na posicao
% pos.
%
% Este predicado eh depois definido com aridade 4
% numeros_comuns(Lst_Perms, Numeros_comuns,Min,N)
% Min -> Caso a lista de permutacoes tenha permutacoes de tamanhos diferentes,
% Min sera o tamanho da permutacao mais pequena
% N -> o indice das permutacoes a analisar (comecando em 1 ate Min)
%********************************************************************************

numeros_comuns(Lst_Perms, Numeros_comuns):-
    maplist(length,Lst_Perms,Size),
    min_member(Min,Size),
    numeros_comuns(Lst_Perms, Numeros_comuns,Min,1).

numeros_comuns(_, [],Min,Min_1):-
    Min_1 is Min+1. 

numeros_comuns([[El1|Res1]|Res], Numeros_comuns,Min,N):-
    N =< Min,
    elimina_primeiros_el(Res,PreNewRes),
    append([Res1],PreNewRes,NewRes),
    NewN is N+1,
    %O que esta em cima e necessario nos dois casos abaixo
    (primeiro_el_igual(El1,Res) ->
        numeros_comuns(NewRes, Nc2,Min,NewN),
        append([(N,El1)],Nc2,Numeros_comuns);
    numeros_comuns(NewRes, Numeros_comuns,Min,NewN)).
    

% 3.1.12  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% unifica_indices(Lst,Numeros_comuns)
% Numeros comuns eh uma lista de listas tal como obtido 
% no predicado numeros_comuns
%
% Unifica as variaveis de Lst, com os valores de Numeros_comuns associado 
% ao indice da variavel que se vai unificar
%
% Este predicado eh depois definido tambem com aridade 3
% unifica_indices(Lst,Numeros_comuns,Ind)
% Ind -> Corresponde ao indice inicial do primeiro elemento de Lst
% (comeca em 1)
% --------------------------------------------------------------------------------


unifica_indices(Lst,Numeros_comuns):-
    %Comecar no indice 1 (primeiro)
    unifica_indices(Lst,Numeros_comuns,1).

unifica_indices(_,[],_).

unifica_indices([H1|Lst],[(Ind,Val)|Numeros_comuns],Ind):-
    H1 = Val,
    IndN is Ind+1,
    unifica_indices(Lst,Numeros_comuns,IndN).

    
unifica_indices([_|Lst],[(Ind1,Val)|Numeros_comuns],Ind2):-
    Ind1 =\= Ind2,
    IndN is Ind2+1,
    unifica_indices(Lst,[(Ind1,Val)|Numeros_comuns],IndN).

%********************************************************************************
% atribui_comuns(Perms_Possiveis)
% Perms_Possiveis eh uma lista de permutacoes possiveis,
% actualiza esta lista atribuindo a cada espaco numeros comuns
% a todas as permutacoes possiveis para esse espaco, 
% tal como descrito na Seccao 2.1, no passo 3a
%********************************************************************************

atribui_comuns([]). 

atribui_comuns([P1|Perms_Possiveis1]):-
    P1 = [Lst,Perms],
    numeros_comuns(Perms, Numeros_comuns),
    unifica_indices(Lst,Numeros_comuns),
    atribui_comuns(Perms_Possiveis1).

% 3.1.13  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% retira_nao_unificaveis(Main,L1,L2)
% Main eh uma lista de variaveis e constantes e L1 eh uma lista de listas
%
% Significa que L2 eh a lista de listas que resulta de remover todas
% as sublistas de L1 que nao unificam com Main
% --------------------------------------------------------------------------------


retira_nao_unificaveis(_,[],[]).

retira_nao_unificaveis(Main,[H1|L1],L2):-
    (subsumes_term(Main,H1) ->
        retira_nao_unificaveis(Main,L1,L3),
        append([H1],L3,L2);
    retira_nao_unificaveis(Main,L1,L2)).
        


%********************************************************************************
% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis eh uma lista de permutacoes possiveis.
%
% Significa que Novas_Perms_Possiveis eh o resultado de tirar 
% permutacoes impossiveis de Perms_Possiveis,
% tal como descrito na Seccao 2.1, no passo 3b.
%********************************************************************************

retira_impossiveis([], []).

retira_impossiveis([[H1,Perm1]|Perms_Possiveis], [[H1,Perm2]|Novas_Perms_Possiveis]):-
    retira_nao_unificaveis(H1,Perm1,Perm2),
    retira_impossiveis(Perms_Possiveis,Novas_Perms_Possiveis).
    


% 3.1.14  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis eh uma lista de permutacoes possiveis
%
% Significa que Novas_Perms_Possiveis eh o resultado de 
% simplificar Perms_Possiveis , ou seja, 
% aplicando os predicados atribui_comuns e retira_impossiveis,
% por esta ordem, ate nao haver mais alteracoes.
%********************************************************************************


simplifica(Perms_Possiveis, Novas_Perms_Possiveis):-
    simplifica(Perms_Possiveis,Perms_Possiveis, Novas_Perms_Possiveis).

simplifica(Perms1,Perms2, Novas_Perms_Possiveis):-
    atribui_comuns(Perms2),
    retira_impossiveis(Perms2,Perms3),
    (Perms3 == Perms1 ->
        Perms3 = Novas_Perms_Possiveis;
    simplifica(Perms3,Perms3,Novas_Perms_Possiveis)).


% 3.1.15  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% inicializa(Puzzle, Perms_Possiveis)
% Puzzle eh um puzzle
%
% Significa que Perms_Possiveis eh a lista de permutacoes possiveis 
% simplificada para Puzzle
%********************************************************************************

inicializa(Puzzle, Perms_Possiveis):-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Pre_Perms),
    simplifica(Pre_Perms, Perms_Possiveis).


% 3.2.1  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%---------------------------------------------------------------------------------
% tamanho_sublistas(L,Slist)
% L eh uma lista de listas
%
% Significa que Slist eh a lista com os tamanhos de cada sublista de L
% --------------------------------------------------------------------------------

tamanho_sublistas([],[]).

tamanho_sublistas([L1|L],[S1|Slist]):-
    length(L1,S1),
    tamanho_sublistas(L,Slist).

%---------------------------------------------------------------------------------
% tamanho_permutacoes(Perms,PermSize)
% Perms eh uma lista de permutacoes
%
% Significa que PermSize eh uma lista com os tamanhos de cada permutacao
% --------------------------------------------------------------------------------

tamanho_permutacoes(Perms_Possiveis,PermSize):-
    maplist(tamanho_sublistas,Perms_Possiveis,PreSize),
    maplist(last,PreSize,PermSize).


%********************************************************************************
% escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% Perms_Possiveis eh uma lista de permutacoes possiveis 
%
% Significa que Escolha eh o elemento de Perms_Possiveis escolhido segundo
% o criterio indicado na Seccao 2.2, no passo 1 do enunciado.
% Se todos os espacos em Perms_Possiveis tiverem associadas listas de
% permutacoes unitarias, o predicado deve devolver "falso".
%********************************************************************************

escolhe_menos_alternativas(Perms_Possiveis, Escolha):-
    tamanho_permutacoes(Perms_Possiveis,Size),
    %verificar se ha pelo menos uma lista de perm com
    % mais do que 1 perm
    include(<(1),Size,Check),
    Check \== [],
    nth1(Ind,Size,ElMin),
    min_member(ElMin,Check),
    ElMin>1, 
    !, % como so queremos o primeiro caso
    nth1(Ind,Perms_Possiveis,Escolha).

% 3.2.2  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% experimenta_perm(Escolha, Perms_Possiveis,Novas_Perms_Possiveis)
% Perms_Possiveis eh uma lista de permutacoes possiveis, e Escolha 
% eh um dos seus elementos (escolhido pelo predicado
% escolhe_menos_alternativas)
%
% Este predicado segue os seguintes passos:
% 1. Sendo Esp e Lst_Perms o espaco e a lista de permutacoes de Escolha,
% respectivamente, escolhe uma permutacao de Lst_Perms, Perm. 
% 2. Unifica Esp com Perm.
% 3. Novas_Perms_Possiveis eh o resultado de substituir, em Perms_Possiveis, o
% elemento Escolha pelo elemento [Esp, [Perm]].
%********************************************************************************

experimenta_perm(Escolha, Perms_Possiveis,Novas_Perms_Possiveis):-
    append([L1,[Escolha],L2],Perms_Possiveis),
    [Esp,Lst_Perms] = Escolha,
    member(Perm,Lst_Perms),
    Esp = Perm, 
    append([L1,[[Esp, [Perm]]],L2],Novas_Perms_Possiveis).



% 3.2.3  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% Perms_Possiveis eh uma lista de permutacoes possiveis.
%
% Significa que Novas_Perms_Possiveis eh o resultado de aplicar o algoritmo
% descrito na Seccao 2.2, no enunciado, a Perms_Possiveis.
%********************************************************************************
    
resolve_aux(P, P):-
    tamanho_permutacoes(P,Size),
    %verificar se nao ha listas de perm com
    % mais do que 1 perm
    include(<(1),Size,Check),
    Check == [].


resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis,Pre1_Novas_Perms_Possiveis),
    simplifica(Pre1_Novas_Perms_Possiveis, Pre2_Novas_Perms_Possiveis),
    resolve_aux(Pre2_Novas_Perms_Possiveis, Novas_Perms_Possiveis).


% 3.3.1  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

%********************************************************************************
% resolve(Puz)
% Puz eh um puzzle
%
% Resolve esse puzzle, isto eh, apos a invocacao
% deste predicado a grelha de Puz tem todas as variaveis substituidas
% por numeros que respeitam as restricoes Puz.
%********************************************************************************


resolve(Puz):- 
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis,_).