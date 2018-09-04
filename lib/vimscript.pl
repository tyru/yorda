:- module(vimscript, [
  op(500, xfy, =>),
  (=>)/2,
  op(500, xfy, ->),
  (->)/2,
  op(500, xfy, @),
  (@)/2,
  eval/4,
  prim/1,
  type_name/2
]).

% ===================== Operators =====================

:- multifile (=>)/2.
:- op(500, xfy, =>).
:- op(500, xfy, ->).
:- op(500, xfy, @).

% ===================== Types =====================

% Primitive types
prim(tAny).
prim(tVoid).
% 42 == tInt(42)
prim(tInt(_)).
% 12.34 == tFloat(12.34)
prim(tFloat(_)).
% "hello" == tString("hello")
prim(tString(_)).
% [42] == tList([tInt(42) @ _])
prim(tList(_)).
% {'foo': 1, 'bar': 2} == tDict([
%   [tString("foo") @ _, tInt(1) @ _],
%   [tString("bar") @ _, tInt(2) @ _]
% ])
prim(tDict(_)).
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
type_name(tList(_), N) :- N = "List", !.
type_name(tDict(_), N) :- N = "Dict", !.
type_name(tTuple, N) :- N = "[]", !.
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
% TODO more tTuple rules

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
  stringify_list(L, "", S).
stringify_list([V|Rest], S, Result) :-
  stringify(V, VS),
  concat_atom([S, VS], "", Added),
  stringify_list(Rest, Added, Result).
stringify_list([], S, Result) :- Result = S.

empty([]).

and(a, b) :- a, b.
and(a, b, c) :- a, b, c.

% abs(tInt(V)) = tInt(V)
% abs(tFloat(V)) = tFloat(V)
vimfunc("abs", [tInt(V) @ _] -> tInt(V) @ _).
vimfunc("abs", [tFloat(V) @ _] -> tFloat(V) @ _).
% acos(tFloat) = tFloat
vimfunc("acos", [tFloat(_) @ _] -> tFloat(_) @ _).
% add(L, E) = [E|L]
vimfunc("add", [tList(L) @ _, E] -> tList([E|L]) @ _).
% and(tInt, tInt) = tInt
vimfunc("and", [tInt(_) @ _, tInt(_) @ _] -> tInt(_) @ _).
% append(tInt | tString, tList | tString)
vimfunc("append", [tInt(_) @ _, tList(_) @ _] -> tInt(0) @ _).
vimfunc("append", [tInt(A) @ _, tString(B) @ SPos] -> R @ _) :-
  vimfunc("append", [tInt(A) @ _, tList([tString(B) @ SPos]) @ _] -> R @ _).
vimfunc("append", [tString(_) @ _, tList(_) @ _] -> tInt(0) @ _).
vimfunc("append", [tString(A) @ _, tString(B) @ SPos] -> R @ _) :-
  vimfunc("append", [tString(A) @ _, tList([tString(B) @ SPos]) @ _] -> R @ _).
% appendbufline(tInt | tString, tInt | tString, tList | tString)
vimfunc("appendbufline", [tInt(_) @ _, tInt(_) @ _, tList(_) @ _] -> tInt(0) @ _).
vimfunc("appendbufline", [tInt(A) @ _, tInt(B) @ _, tString(C) @ SPos] -> R @ _) :-
  vimfunc("appendbufline", [tInt(A) @ _, tInt(B) @ _, tList([tString(C) @ SPos]) @ _] -> R @ _).
vimfunc("appendbufline", [tInt(_) @ _, tString(_) @ _, tList(_) @ _] -> tInt(0) @ _).
vimfunc("appendbufline", [tInt(A) @ _, tString(B) @ _, tString(C) @ SPos] -> R @ _) :-
  vimfunc("appendbufline", [tInt(A) @ _, tString(B) @ _, tList([tString(C) @ SPos]) @ _] -> R @ _).
vimfunc("appendbufline", [tString(_) @ _, tInt(_) @ _, tList(_) @ _] -> tInt(0) @ _).
vimfunc("appendbufline", [tString(A) @ _, tInt(B) @ _, tString(C) @ SPos] -> R @ _) :-
  vimfunc("appendbufline", [tString(A) @ _, tInt(B) @ _, tList([tString(C) @ SPos]) @ _] -> R @ _).
vimfunc("appendbufline", [tString(_) @ _, tString(_) @ _, tList(_) @ _] -> tInt(0) @ _).
vimfunc("appendbufline", [tString(A) @ _, tString(B) @ _, tString(C) @ SPos] -> R @ _) :-
  vimfunc("appendbufline", [tString(A) @ _, tString(B) @ _, tList([tString(C) @ SPos]) @ _] -> R @ _).
% has(tString) = tInt(1) | tInt(0)
vimfunc("has", [tString(_) @ _] -> tInt(1) @ _).
vimfunc("has", [tString(_) @ _] -> tInt(0) @ _).

% TODO generalization
funcall(Args -> R, Args, R).

evallist(_, [], _, []).
evallist(Env, [E|Xs1], RetEnv, [R|Xs2]) :-
  eval(Env, E, Env1, R),
  evallist(Env1, Xs1, RetEnv, Xs2).

% ===================== Vim script syntax =====================

% Some bare words become v: variables for compatibility (e.g. "count" -> "v:count")
compat("count", tInt(_)) :- !.           % v:count
compat("errmsg", tInt(_)) :- !.          % v:errmsg
compat("shell_error", tInt(_)) :- !.     % v:shell_error
compat("this_session", tInt(_)) :- !.    % v:this_session
compat("version", tInt(_)) :- !.         % v:version

% Primitive types (with position)
eval(Env, T @ Pos, Env, R @ Pos) :- prim(T), R = T.

% ident(Scope, Name, Lv) @ Pos
% Look up ident variable from env.
% variables and functions have different namespace.
% For example, below code outputs "1", and "42".
%
%		let has = 42
%		echo has("eval")
%		echo has
%
% See call(Fun, Args) for the look-up of functions.
%
% Top-level bare word is a global variable.
eval(Env, ident("", Name, 0) @ Pos, Env, R) :-
  not(compat(Name, _)),
  eval(Env, ident("g", Name, 0) @ Pos, Env, R).
% Non-top-level bare word is a local variable.
eval(Env, ident("", Name, Lv) @ Pos, Env, R) :-
  Lv > 0,
  not(compat(Name, _)),
  eval(Env, ident("l", Name, Lv) @ Pos, Env, R).
% Certain name of bare word is a compat v: variable.
eval(Env, ident("", Name, _) @ Pos, Env, R) :-
  compat(Name, T),
  R = T @ Pos,
  !.
eval([ident(Scope, Name, Lv) @ _ => Rhs | _], ident(Scope, Name, Lv) @ _, _, Rhs) :- !.
eval([ident(_, _, _) @ _ => _ | Env], ident(Scope2, N2, Lv2) @ IdentPos, _, R) :-
  eval(Env, ident(Scope2, N2, Lv2) @ IdentPos, _, R).

% call(Fun, Args) @ Pos
eval(Env, call(ident(_, Name, _) @ _, Args) @ CallPos, Env, R @ CallPos) :-
  vimfunc(Name, FunT),
  funcall(FunT, Args, R @ _),
  !.
% eval(Env, call(ident(Scope, Name, Lv) @ IdentPos, Args) @ CallPos, Env, R @ CallPos) :-

% file(Excmds) @ Pos
eval(Env, file([]) @ _, Env, _).
eval(Env, file([E1 @ E1Pos | Xs]) @ Pos, RetEnv, tVoid @ Pos) :-
  eval(Env, E1 @ E1Pos, Env1, tVoid @ E1Pos),
  eval(Env1, file(Xs) @ Pos, RetEnv, tVoid @ Pos).

% let(Lhs, =, Rhs) @ Pos
eval(Env, let(ident(Scope, Name, Lv) @ IdentPos, =, Rhs) @ LetPos, [ident(Scope, Name, Lv) @ IdentPos => Rhs | Env], tVoid @ LetPos).

% echo(ExprList) @ Pos
eval(Env, echo([]) @ Pos, Env, tVoid @ Pos).
eval(Env, echo([E|Xs]) @ Pos, Env, tVoid @ Pos) :-
  eval(Env, E, Env, _),
  eval(Env, echo(Xs) @ Pos, Env, tVoid @ Pos).
