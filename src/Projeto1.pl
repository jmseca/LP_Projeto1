% Algo a ser escrit

remove_repetidos([], []).

remove_repetidos([P|R], L) :-
  membro(P, R), !, % caso seja membro, nem precisa de ir à cláusula seguinte
  remove_repetidos(R, L).

remove_repetidos([P|R], [P|L]) :- remove_repetidos(R, L).