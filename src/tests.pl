
:- [codigo_comum].

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
    append(PreComb2,PreComb3)
    sort(PreComb3,Combs).