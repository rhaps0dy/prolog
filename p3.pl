lookup(_, void, _) :- false.
lookup(N, dic(N, V, _, _), V).
lookup(N, dic(Ni, _, D1, _ ), V) :- N @< Ni, lookup(N, D1, V).
lookup(N, dic(Ni, _, _ , D2), V) :- N @> Ni, lookup(N, D2, V).

% encodestatement(source, dict, target).
encodestatement(assign(name(X),Expr), D, (E_code; instr(store,Addr))) :-
	encodeexpr(Expr, D, E_code), lookup(X, D, Addr).

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
encodeexpr(const(X), D, instr(loadc, X)).
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

%% Pruebas para el programa
% encodestatement(assign(name(x),expr(+,name(x),const(a))),D,X).
% encodestatement(assign(name(x),name(y)),D,X).
% encodestatement(assign(name(x),expr(+,name(x),const(a))),D,X).
% encodeexpr(expr('/', expr(+, expr(*, const(5), name(a)), name(b)), name(c)))
