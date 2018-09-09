:- module(vimscript, [
  op(500, xfy, ::),
  (::)/2,
  op(600, xfy, @),
  (@)/2,
  op(700, xfy, =>),
  (=>)/2,
  new_env/1,
  add_var/4,
  get_var/4,
  get_level/2,
  eval_expr/2,
  eval/4,
  prim/1
]).

% ===================== Operators =====================

:- multifile (::)/2.
:- multifile (@)/2.
:- multifile (=>)/2.
:- op(500, xfy, ::).
:- op(600, xfy, @).
:- op(700, xfy, =>).

% ===================== Utilities =====================

empty([]).

% ===================== Primitive types =====================

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

% ===================== Built-in functions =====================

% abs(tInt(V)) = tInt(V)
% abs(tFloat(V)) = tFloat(V)
vimfunc(_, "abs", [tInt(V) @ _] :: tInt(V) @ _).
vimfunc(_, "abs", [tFloat(V) @ _] :: tFloat(V) @ _).
% acos(tFloat) = tFloat
vimfunc(_, "acos", [tFloat(_) @ _] :: tFloat(_) @ _).
% add(L, E) = [E|L]
vimfunc(_, "add", [tList(L) @ _, E] :: tList([E|L]) @ _).
% and(tInt, tInt) = tInt
vimfunc(_, "and", [tInt(_) @ _, tInt(_) @ _] :: tInt(_) @ _).
% append(tInt | tString, tList | tString) = tInt(0)
vimfunc(_, "append", [tInt(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "append", [tInt(A) @ _, tString(B) @ SPos] :: R @ _) :-
  vimfunc(Env, "append", [tInt(A) @ _, tList([tString(B) @ SPos]) @ _] :: R @ _).
vimfunc(_, "append", [tString(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "append", [tString(A) @ _, tString(B) @ SPos] :: R @ _) :-
  vimfunc(Env, "append", [tString(A) @ _, tList([tString(B) @ SPos]) @ _] :: R @ _).
% appendbufline(tInt | tString, tInt | tString, tList | tString) = tInt(0)
vimfunc(_, "appendbufline", [tInt(_) @ _, tInt(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "appendbufline", [tInt(A) @ _, tInt(B) @ _, tString(C) @ SPos] :: R @ _) :-
  vimfunc(Env, "appendbufline", [tInt(A) @ _, tInt(B) @ _, tList([tString(C) @ SPos]) @ _] :: R @ _).
vimfunc(_, "appendbufline", [tInt(_) @ _, tString(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "appendbufline", [tInt(A) @ _, tString(B) @ _, tString(C) @ SPos] :: R @ _) :-
  vimfunc(Env, "appendbufline", [tInt(A) @ _, tString(B) @ _, tList([tString(C) @ SPos]) @ _] :: R @ _).
vimfunc(_, "appendbufline", [tString(_) @ _, tInt(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "appendbufline", [tString(A) @ _, tInt(B) @ _, tString(C) @ SPos] :: R @ _) :-
  vimfunc(Env, "appendbufline", [tString(A) @ _, tInt(B) @ _, tList([tString(C) @ SPos]) @ _] :: R @ _).
vimfunc(_, "appendbufline", [tString(_) @ _, tString(_) @ _, tList(_) @ _] :: tInt(0) @ _).
vimfunc(Env, "appendbufline", [tString(A) @ _, tString(B) @ _, tString(C) @ SPos] :: R @ _) :-
  vimfunc(Env, "appendbufline", [tString(A) @ _, tString(B) @ _, tList([tString(C) @ SPos]) @ _] :: R @ _).
% call(X, Args, Self)
% add Self to Env, call call(X, Args) recursively.
vimfunc(Env, "call", [X, tList(Args) @ _, Self] :: R @ _) :-
  NextEnv = [ident("l", "self") @ _ => Self | Env],
  vimfunc(NextEnv, "call", [X, tList(Args) @ _] :: R @ _),
  !.
% call(Fun, Args)
vimfunc(Env, "call", [Fun, tList(Args) @ _] :: R @ _) :-
  eval(Env, call(Fun, Args) @ _, _, R @ _),
  !.
% Name must not be a variable, and must be a vimfunc.
vimfunc(_, "function", [tString(Name) @ _] :: R @ _) :-
  var(Name),
  R = _ :: _,
  !.
vimfunc(Env, "function", [tString(Name) @ _] :: FunT @ _) :-
  not(var(Name)),
  vimfunc(Env, Name, FunT @ _),
  !.
% tr(tString, tString, tString) = tString
vimfunc(_, "tr", [tString(_) @ _, tString(_) @ _, tString(_) @ _] :: tString(_) @ _) :- !.
% has(tString) = tInt(1) | tInt(0)
vimfunc(_, "has", [tString(_) @ _] :: tInt(1) @ _).
vimfunc(_, "has", [tString(_) @ _] :: tInt(0) @ _).

% ===================== Compat variables =====================

% Some bare words become v: variables for compatibility (e.g. "count" :: "v:count")
compat("count", "v", tInt(_)) :- !.           % v:count
compat("errmsg", "v", tInt(_)) :- !.          % v:errmsg
compat("shell_error", "v", tInt(_)) :- !.     % v:shell_error
compat("this_session", "v", tInt(_)) :- !.    % v:this_session
compat("version", "v", tInt(_)) :- !.         % v:version

% ===================== Env =====================

% new_env(-Env)
new_env([lv:0, vars:[]]).

% add_var(+Env, ident(+Scope, +Name) @ +Pos, +Rhs, -RetEnv)
add_var(Env, ident("", Name) @ Pos, Rhs, RetEnv) :-
  add_scope(Env, Name, Scope),
  add_var(Env, ident(Scope, Name) @ Pos, Rhs, RetEnv).
add_var(Env, ident(Scope, Name) @ Pos, Rhs, RetEnv) :-
  \+ Scope = "",
  Env = [lv:Lv, vars:Vars],
  RetEnv = [lv:Lv, vars:[ident(Scope, Name) @ Pos => Rhs | Vars]].

% get_var(+Env, ident(?Scope, ?Name), -Pos, -Rhs)
get_var(Env, ident(InScope, Name), Pos, Rhs) :-
  ((nonvar(Name), nonvar(InScope), InScope = "") ->
    add_scope(Env, Name, Scope);
    Scope = InScope),
  get_vars(Env, Vars),
  member(ident(Scope, Name) @ Pos => Rhs, Vars).

% add_scope(+Env, +Name, -Scope)
add_scope(Env, Name, Scope) :-
  nonvar(Env), nonvar(Name),
  compat(Name, Scope, _), !;
  get_level(Env, Lv),
  Lv > 0 -> Scope = "l"; Scope = "g".

% get_vars(+Env, -Vars)
get_vars([lv:_, vars:Vars], Vars).

% get_level(+Env, -Lv)
get_level([lv:Lv, vars:_], Lv).

% ===================== Vim script syntax =====================

% For convenience
eval_expr(T, R) :- new_env(Env), eval(Env, T, Env, R).

% Primitive types (with position)
eval(Env, T @ Pos, Env, R @ Pos) :- prim(T), R = T.

% ident(Scope, Name) @ Pos
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
eval(Env, ident(Scope, Name) @ _, _, Rhs) :-
  get_var(Env, ident(Scope, Name), _, Rhs).

% call(Fun, Args) @ Pos
eval(Env, call(Args :: R @ _, Args) @ CallPos, Env, R @ CallPos) :- !.
% Vim built-in function (e.g. has("eval"))
eval(Env, call(ident("", Name) @ _, Args) @ CallPos, Env, R @ CallPos) :-
  vimfunc(Env, Name, Args :: R @ _),
  !.
% Variable is a funcref
%
%		" F = ident(Scope, Name) @ _
%		let F = function('has')
%		call F('eval')
%		let g:has = function('has')
%		call g:has('eval')
%
eval(Env, call(ident(Scope, Name) @ _, Args) @ CallPos, _, R @ CallPos) :-
  not((Scope = "", vimfunc(Env, Name, _))),
  eval(Env, ident(Scope, Name) @ _, Env, FunT @ _),
  eval(Env, call(FunT @ _, Args) @ CallPos, Env, R @ _),
  !.
% Expression is a funcref
%
%		" function('has') = call(ident(Scope, Name) @ _, InnerArgs) @ _
%		call function('has')('eval')
%
eval(Env, call(call(ident(Scope, Name) @ _, InnerArgs) @ _, Args) @ CallPos, Env, R @ CallPos) :-
  eval(Env, call(ident(Scope, Name) @ _, InnerArgs) @ _, Env, FunT @ _),
  eval(Env, call(FunT @ _, Args) @ CallPos, Env, R @ _),
  !.

% file(Excmds) @ Pos
eval(Env, file([]) @ _, Env, _) :- !.
eval(Env, file([E1 @ E1Pos | Xs]) @ Pos, RetEnv, tVoid @ Pos) :-
  eval(Env, E1 @ E1Pos, Env1, tVoid @ E1Pos),
  eval(Env1, file(Xs) @ Pos, RetEnv, tVoid @ Pos),
  !.

% let(Lhs, =, Rhs) @ Pos
eval(Env, let(ident(Scope, Name) @ IdentPos, =, Rhs) @ LetPos, RetEnv, tVoid @ LetPos) :-
  add_var(Env, ident(Scope, Name) @ IdentPos, Rhs, RetEnv),
  !.

% echo(ExprList) @ Pos
eval(Env, echo([]) @ Pos, Env, tVoid @ Pos) :- !.
eval(Env, echo([E|Xs]) @ Pos, Env, tVoid @ Pos) :-
  eval(Env, E, Env, _),
  eval(Env, echo(Xs) @ Pos, Env, tVoid @ Pos),
  !.
