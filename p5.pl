lookup(_, void, _) :- false.
lookup(N, dic(N, V, _, _), V).
lookup(N, dic(Ni, _, D1, _ ), V) :- N @< Ni, lookup(N, D1, V).
lookup(N, dic(Ni, _, _ , D2), V) :- N @> Ni, lookup(N, D2, V).
lookup(Name,dic(Name,Value,_,_),Value) :- !.

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

% compilation
compile(Source, (Code; instr(halt,0); block(L)) ) :-
	encodestatement(Source,D,Code),
	assemble(Code,1,N0),
	N1 is N0+1,
	allocate(D,N1,N),
	L is N-N1.

assemble((Code1;Code2),N0,N1) :- assemble(Code1,N0,N2), assemble(Code2,N2,N1).
assemble(instr(_,_),N0,N) :- N is N0+1.
assemble(label(N),N,N).

allocate(void,N, N) :- !.
allocate(dic(Name,N1,Before,After),N0,N) :-
	allocate(Before,N0,N1),
	N2 is N1+1,
	allocate(After,N2,N).

puts([]).
puts([X|L]) :- put(X), puts(L).

% Pretty print
prettycompile(Source) :-
	puts("\tADDR\tINST\tOP\n"),
	compile(Source, Code),
	printcode(1, Code, N),
	puts("\t"), write(N), puts("\n").

printcode(N, (Code1; Code2), N3) :- printcode(N, Code1, N2), printcode(N2, Code2, N3).
printcode(N, instr(Ins,Op), N2) :- puts("\t"), write(N), puts("\t"), write(Ins), puts("\t"), write(Op), puts("\n"), N2 is N+1.
printcode(N, block(L), N2) :- puts("\t"), write(N), puts("\tBLOCK\t"), write(L), puts("\n"), N2 is N+L.
printcode(N, label(L), N) :- puts("LABEL").

% Tests
% prettycompile(if(test(=,name(x),const(5)),assign(name(x),const(1)), assign(name(x),const(2)))).

% prettycompile(while(test(=,name(x),const(5)),assign(name(x),expr(+,name(x),const(1))))).

% prettycompile((read(name(v));
%                assign(name(c), const(1));
%                assign(name(r),const(1));
%                while(test(<,name(c),name(v)),
%                      assign(name(c),expr(+,name(c),const(1)));
%                      assign(name(r),expr(*,name(r),name(c))));
%                write(name(r)))).


% El programa de la practica anterior
% prettycompile((read(name(x));
%                  assign(name(n),const(1));
%                  while(test('>', name(x), const(1)),
%                        (assign(name(n), expr('*', name(n), name(x)));
%                         assign(name(x), expr('-', name(x), const(1)))));
%                  write(name(n)))).