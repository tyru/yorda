:- module(vimscript, [
  op(500, xfy, @),
  (@)/2,
  eval/2,
  eval/4,
  prim/1,
  type_name/2
]).

% ===================== Operators =====================

:- op(500, xfy, @).

% ===================== Types =====================

% Primitive types
prim(tAny).
prim(tVoid).
prim(tInt(_)).
prim(tFloat(_)).
prim(tString(_)).
prim(tList(_)).
prim(tDict(_, _)).
prim(tTuple).
prim(tTuple(_)).
prim(tTuple(_, _)).
prim(tTuple(_, _, _)).
prim(tTuple(_, _, _, _)).
prim(tTuple(_, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).
prim(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)).
prim(tError(_, _)).


% ===================== Utilities =====================

% for variable (e.g. type_name(T, N))
type_name(unknown, N) :- N = "Unknown", !.
type_name(tAny, N) :- N = "Any", !.
type_name(tVoid, N) :- N = "Void", !.
type_name(tInt(_), N) :- N = "Int", !.
type_name(tFloat(_), N) :- N = "Float", !.
type_name(tString(_), N) :- N = "String", !.
type_name(tList(T), N) :-
  type_name(T, TN),
  concat_atom(["List<", TN, ">"], "", N),
  !.
type_name(tDict(K, V), N) :-
  type_name(K, KName),
  type_name(V, VName),
  concat_atom(["Dict<", KName, ", ", VName, ">"], "", N),
  !.
type_name(tTuple(T1), N) :-
  type_name(T1, T1Name),
  concat_atom(["[", T1Name, "]"], "", N),
  !.
type_name(tTuple(T1, T2), N) :-
  type_name(T1, T1Name),
  type_name(T2, T2Name),
  concat_atom(["[", T1Name, ", ", T2Name, "]"], "", N),
  !.
type_name(tTuple(T1, T2, T3), N) :-
  type_name(T1, T1Name),
  type_name(T2, T2Name),
  type_name(T3, T3Name),
  concat_atom(["[", T1Name, ", ", T2Name, ", ", T3Name, "]"], "", N),
  !.
% for wrong prim (e.g. type_name("foo", N))
type_name(S, N) :-
  string(S),
  N = S,
  !.
% for wrong prim (e.g. type_name(foo, N))
type_name(Atom, N) :- atom_string(Atom, N).

error(Expr, Msg, Err) :- prim(Err) = prim(tError(Expr, Msg)).
error_string(Expr, Expected, Got, Err) :-
  stringify(Expected, E),
  stringify(Got, G),
  concat_atom(["expected ", E, " but got ", G], Atom),
  atom_string(Atom, Msg),
  error(Expr, Msg, Err).

stringify(S1, S2) :-
  string(S1),
  S2 = S1.
stringify(T, S) :-
  prim(T),
  type_name(T, S).
stringify(V, S) :-
  term_string(V, S).
stringify(L, S) :-
  is_list(L),
  stringify(L, "", S).
stringify([V|Rest], S, Result) :-
  (string(V); prim(V)),
  stringify(V, VS),
  concat_atom([S, VS], "", Added),
  stringify(Rest, Added, Result).
stringify([], S, Result) :- Result = S.

% ===================== Vim script syntax =====================

% For convenience
eval(E, R) :- eval([], E, _, R).

% Primitive types (with position)
eval(Env, T @ Pos, Env, R @ Pos) :- prim(T), R = T.

% ident(Name, Rhs) @ Pos
% Look up ident from env.
eval([ident(Name, Rhs) @ VarPos | _], ident(Name, Rhs) @ _, _, Rhs @ VarPos).
eval([ident(K, _) | Env], ident(Name, Rhs) @ Pos, _, R) :-
  K \= Name,
  eval(Env, ident(Name, Rhs) @ Pos, _, R).

% file(Excmds) @ Pos
eval(Env, file([]) @ _, Env, _).
eval(Env, file([E1 @ E1Pos | Xs]) @ Pos, RetEnv, tVoid @ Pos) :-
  eval(Env, E1 @ E1Pos, Env1, tVoid @ E1Pos),
  eval(Env1, file(Xs) @ Pos, RetEnv, tVoid @ Pos).

% let(Lhs, =, Rhs) @ Pos
eval(Env, let(ident(Name, Rhs) @ IdentPos, =, Rhs) @ LetPos, [ident(Name, Rhs) @ IdentPos | Env], tVoid @ LetPos).

% echo(ExprList) @ Pos
eval(Env, echo([]) @ Pos, Env, tVoid @ Pos).
eval(Env, echo([E|Xs]) @ Pos, Env, tVoid @ Pos) :-
  eval(Env, E, Env, _),
  eval(Env, echo(Xs) @ Pos, Env, tVoid @ Pos).
