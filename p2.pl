%%% Ejercicio 1

initial(1).
final(4).
arc(1, 2, C) :- char_code(j, C).
arc(2, 1, C) :- char_code(a, C). 
arc(2, 3, C) :- char_code(a, C).
arc(3, 4, C) :- char_code('!', C).

% Construimos una lista de listas en vez de una sola lista con append
% porque es bastante más legible.
recognize([], X, []) :- final(X).
recognize([C|R], X, [[X, S, Y]|O]) :- recognize(R, Y, O), arc(X, Y, C), char_code(S, C).
recognize(L, O) :- initial(X), recognize(L, X, O).

% Ejemplo de uso:
% recognize("jajaja!", XD).

%%% Ejercicio 2
:- op(100, xfx, ::=).
e     ::= [g, e1].
e1    ::= ['+', g, e1].
e1    ::= [].
g     ::= [f, g1].
g1    ::= ['*', f, g1].
g1    ::= [].
f     ::= ['(', e, ')'].
('(') ::= "(".
(')') ::= ")".
('+') ::= "+".
('*') ::= "*".
f     ::= "a".
f     ::= "b".
f     ::= "c".
f     ::= "d".

% Predicado para poder imprimir las strings como atomos
list_of_ints([]).
list_of_ints([H|L]) :- integer(H), list_of_ints(L).
printable(Sp, S) :- list_of_ints(S), atom_codes(Sp, S).
printable(S, S).

% parse devuelve la lista de reglas usadas
parse([], [], []).
parse([X|R], [X|R1], O) :- parse(R, R1, O).
parse(L, [X|R], [[X, Sprintable]|O]) :- X ::= S, append(S, R, R1), parse(L, R1, O), printable(Sprintable, S).

parse(A, O) :- X ::= _, parse(A, [X], O).

% Ejemplo de uso: parse("(a+b)*c", X).

%%% Ejercicio 3
dic(sal, sel,
    dic(mostassa, moutard,
	void,
	dic(pebre, poivre, void, void)),
    dic(vinagre, vinaigre, void, void)).

look(Name,Value) :- dic(N, V, D1, D2), lookup(Name, dic(N, V, D1, D2), Value).
lookup(_, void, _) :- false.
lookup(N, dic(N, V, _, _), V).
lookup(N, dic(Ni, _, D1, _ ), V) :- N @< Ni, lookup(N, D1, V).
lookup(N, dic(Ni, _, _ , D2), V) :- N @> Ni, lookup(N, D2, V).

% Ejemplos de uso:
% look(vinagre, V).
% lookup(y,D,Ay), lookup(x,D,21), lookup(z,D,23).