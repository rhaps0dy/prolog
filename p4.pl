lookup(_, void, _) :- false.
lookup(N, dic(N, V, _, _), V).
lookup(N, dic(Ni, _, D1, _ ), V) :- N @< Ni, lookup(N, D1, V).
lookup(N, dic(Ni, _, _ , D2), V) :- N @> Ni, lookup(N, D2, V).

% Definicion operaciones
opv(+, add).
opv(-, sub).
opv(*, mul).
opv('/', div).
opc(+, addc).
opc(-, subc).
opc(*, mulc).
opc('/', divc).

% encodeexpr(Expr,D,E_code).
encodeexpr(const(X), _, instr(loadc, X)).
encodeexpr(name(X), D, instr(load, Addr)) :- lookup(X, D, Addr).

encodeexpr(expr(Sym, Expr, name(X)), D,
	   (E_code; instr(Op, Addr))) :-
	opv(Sym, Op),
	encodeexpr(Expr, D, E_code),
	lookup(X, D, Addr).

encodeexpr(expr(Sym, Expr, const(X)), D,
	   (E_code; instr(Op, X))) :-
	opc(Sym, Op),
	encodeexpr(Expr, D, E_code).

opcomp('=', jumpne).
opcomp('/=', jumpeq).
opcomp('>', jumple).
opcomp('<', jumpge).
opcomp('>=', jumplt).
opcomp('<=', jumpgt).

% Store second expression in memory first. a*b-c*d approach here does not work,
% it would evaluate to ((a*b)-c)*d
% the temporary value is stored on variable reserved-test, which shouldn't be
% used anywhere else in the program.
encodetest(test(Op,A1,A2),D,L, (A2code; Subcode; instr(J,L))) :-
	encodestatement(assign(name(reserved-test), A2), D, A2code),
	encodeexpr(expr('-', A1, name(reserved-test)), D, Subcode),
	opcomp(Op, J).

% if-then-else
encodestatement(if(Test,Then,Else),D,
		(Testcode; Thencode; instr(jump,L2);
		label(L1); Elsecode; label(L2))) :-
		encodetest(Test,D,L1,Testcode),
		encodestatement(Then,D,Thencode),
		encodestatement(Else,D,Elsecode).

% Assignment
encodestatement(assign(name(X),Expr), D, (E_code; instr(store,Addr))) :-
	encodeexpr(Expr, D, E_code), lookup(X, D, Addr).

% Join statements with ;
encodestatement((S1;S2),D,(Code1;Code2)) :-
	encodestatement(S1, D, Code1),
	encodestatement(S2, D, Code2).

% While
encodestatement(while(Test,Body),D,
		(label(L1); Testcode; Bodycode; instr(jump,L1); label(L2))) :-
	encodetest(Test,D,L2,Testcode),
	encodestatement(Body,D,Bodycode).

% Read and write
encodestatement(read(name(X)),D,instr(read,Addr)) :- lookup(X, D, Addr).
encodestatement(write(Expr),D,(Ecode;instr(write,0))) :- encodeexpr(Expr, D, Ecode).

% Skip
encodestatement(skip, D, (;)).


% Algunos tests
% encodestatement(if(test(=,name(x),const(5)), assign(name(x),const(1)), assign(name(x),const(2))),D,X).
% encodestatement(while(test(=,name(x),const(5)), assign(name(x),expr(+,name(x),const(1)))),D,X).
% encodestatement((read(name(x));write(expr(+,name(x),const(1)))),D,X).
% encodestatement(if(test('>',expr('*', name(x), name(a)),const(5)), skip, skip), D, X).

% Programa que calcula el factorial de un numero
% encodestatement((read(name(x));
%                  assign(name(n),const(1));
%                  while(test('>', name(x), const(1)),
%                        (assign(name(n), expr('*', name(n), name(x)));
%                         assign(name(x), expr('-', name(x), const(1)))));
%                  write(name(n))),
%                 Diccionario, Instrucciones).

% La respuesta es algo dificil de leer tal y como sale de swipl
% asi que la reproduzco estructurada aqui
%
% Diccionario = dic(x, _G2847,
% 		  dic(n, _G2861, _G2868, _G2869),
% 		  dic(temp-test, _G2914, _G2921, _G2922))
%
% Instrucciones =
% (instr(read, _G2847);
%  (instr(loadc, 1);
%   instr(store, _G2861));
%  (label(_G2877);
%   ((instr(loadc, 1);
%     instr(store, _G2914));
%    (instr(load, _G2847);
%     instr(sub, _G2914));
%    instr(jumple, _G2891));
%   (((instr(..., ...);        % me gustaria ver que pasa aqui...
%      instr(..., ...));
%     instr(store, _G2861));
%    (instr(..., ...);
%     instr(..., ...));
%    instr(store, _G2847));
%   instr(jump, _G2877);
%   label(_G2891));
%  instr(load, _G2861);
%  instr(write, 0)) .