%%% Pregunta 1: que lenguaje genera la GLC?
% La GLC genera expresiones aritmeticas con las variables a, b, c, d y
% las operaciones * y +. Son validas todas las sumas de
% multiplicaciones, la gramatica incorpora en ella la precedencia de
% la multiplicacion sobre la suma. Tambien se pueden utilizar
% parentesis con una expresion en el interior en lugar de cualquier
% variable.

%%% Parser GLC Aritmetica
% Definimos operador ::= con precedencia muy preferente, de tipo infijo
:- op(100, xfx, ::=).

% GLC expresiones aritmeticas, limitadas
% No-terminales son atomos.
e     ::= [g, e1].
e1    ::= ['+', g, e1].
e1    ::= [].
g     ::= [f, g1].
g1    ::= ['*', f, g1].
g1    ::= [].
f     ::= ['(', e, ')'].
% Terminales son constantes string (listas de enteros)
('(') ::= "(".
(')') ::= ")".
('+') ::= "+".
('*') ::= "*".
f     ::= "a".
f     ::= "b".
f     ::= "c".
f     ::= "d".

% parse/2(Input, Stack) parsea una string con la gramatica definida.
parse([], []).
% X es forzosamente un terminal, ya que aparece en Input
parse([X|R], [X|R1]) :- parse(R, R1).
% X es forzosamente no terminal, aparece en la izquierda en la GLC
parse(L, [X|R]) :- X ::= S, append(S, R, R1), parse(L, R1).

% Al pedir que sea una regla cualquiera, parse/1 coge la primera.
parse(A) :- X ::= _, parse(A, [X]).

%%% Ejemplos de consultas
%%%% dan no
% parse("+ab").
% parse("abcd").
% parse("a++b").
% parse("*a+b").
% parse("a+b)").
%%%% dan yes
% parse("a+b").
% parse("(a+b)*c").
% parse("a*c+d*b").
% parse("a*(b+c*d)*c").
% parse("a+b*c+(d*a+c)"). 


% Automata
initial(1).
final(4).
% j = 106, a = 97, ! = 33
arc(1, 2, 106).
arc(2, 1, 97).
arc(2, 3, 97).
arc(3, 4, 33).

recognize([], X) :- final(X).
recognize([S|R], X) :- arc(X, Y, S), recognize(R, Y).
recognize(L) :- initial(X), recognize(L, X).

%%% Ejemplos de consultas
%%%% dan no
% recognize("ajaja").
% recognize("ajaja!").
% recognize("a!jaja").
% recognize("jaja").
%%%% dan si
% recognize("ja!").
% recognize("jajajaja!").
% recognize("jajaja!").

generate(X) :- initial(Y), generate(Y, []).
generate(Y, X) :- 
