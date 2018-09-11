:- module(vimscript, [
  op(500, xfy, ::),
  (::)/2,
  op(600, xfy, @),
  (@)/2,
  op(700, xfy, =>),
  (=>)/2,
  new_env/1,
  eval_expr/2,
  eval_expr/3,
  eval/4
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
% v:true == tBool(true), v:false == tBool(false)
prim(tBool(_)).
% v:null == tNone(null), v:none == tNone(none)
prim(tNone(_)).
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

to_bool(tBool(V), tBool(V)).
to_bool(tNone(_), tBool(false)).
to_bool(tInt(_), tBool(_)).
to_bool(tString(_), tBool(_)).
to_bool(tAny, tBool(_)).

to_string(tString(V), tString(V)).
to_string(tBool(_), tString(_)).
to_string(tNone(_), tString(_)).
to_string(tInt(_), tString(_)).
to_string(tAny, tString(_)).

to_int(tInt(V), tInt(V)).
to_int(tString(_), tInt(_)).
to_int(tBool(_), tInt(_)).
to_int(tNone(_), tInt(_)).
to_int(tAny, tInt(_)).

get_prop(tAny, _, tAny).
get_prop(_, tAny, tAny).
get_prop(tDict(_), Right, tAny) :- to_string(Right, _).
get_prop(tString(_), Right, tAny) :- to_int(Right, _).

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
new_env([hooks:[], lv:0, vars:[], funcs:[]]).

% run_hook(+Env, +Hook)
run_hook(_, _).

% get_level(+Env, -Lv)
get_level([hooks:_, lv:Lv, vars:_, funcs:_], Lv).

% get_vars(+Env, -Vars)
get_vars([hooks:_, lv:_, vars:Vars, funcs:_], Vars).

% update_var(+Env, +Vars, -UpdateVars, -RetEnv)
update_var(
  [hooks:Hooks, lv:Lv, vars:Vars, funcs:Funcs],
  Vars,
  UpdateVars,
  [hooks:Hooks, lv:Lv, vars:UpdateVars, funcs:Funcs]
).

% update_func(+Env, +Funcs, -UpdateFuncs, -RetEnv)
update_func(
  [hooks:Hooks, lv:Lv, vars:Vars, funcs:Funcs],
  Funcs,
  UpdateFuncs,
  [hooks:Hooks, lv:Lv, vars:Vars, funcs:UpdateFuncs]
).

% get_funcs(+Env, -Funcs)
get_funcs([hooks:_, lv:_, vars:_, funcs:Funcs], Funcs).

% add_func(+Env, function(+Name, +Params, +Body) @ +Pos, -RetEnv)
add_func(Env, function(Name, Params, Body) @ Pos, RetEnv) :-
  add_params(Env, Params, Env1),
  update_func(Env1, Funcs, [function(Name, Params, Body) @ Pos | Funcs], RetEnv).

% add_params(+Env, +Params, -RetEnv)
add_params(Env, Params, RetEnv) :-
  foldl(add_param1, Params, Env, RetEnv).
add_param1(ident("", Name) @ Pos, Env, RetEnv) :-
  add_var(Env, ident("a", Name) @ Pos, _, RetEnv).

% call_func(+Env, call(+Fun, +Args), -R)
call_func(_, call(Args :: R @ _, Args), R).
% No scope, must be a Vim built-in function or a variable (e.g. has("eval"), F(42))
call_func(Env, call(ident("", Name) @ _, Args), R) :-
  vimfunc(Env, Name, Args :: R @ _), !;
  add_scope(Env, Name, Scope),
  call_func(Env, call(ident(Scope, Name) @ _, Args), R),
  !.
% A variable (with scope) is a funcref (e.g. l:F(42), g:F(42))
call_func(Env, call(ident(Scope, Name) @ _, Args), R) :-
  \+ Scope = "",
  eval_expr(Env, ident(Scope, Name) @ _, FunT @ _),
  call_func(Env, call(FunT @ _, Args), R),
  !.
% Expression is a funcref
%
%		" function('has') = call(ident(Scope, Name) @ _, InnerArgs) @ _
%		call function('has')('eval')
%
call_func(Env, call(call(ident(Scope, Name) @ _, InnerArgs) @ _, Args), R) :-
  call_func(Env, call(ident(Scope, Name) @ _, InnerArgs), FunT),
  call_func(Env, call(FunT @ _, Args), R).

% add_var(+Env, ident(+Scope, +Name) @ +Pos, +Rhs, -RetEnv)
add_var(Env, ident("", Name) @ Pos, Rhs, RetEnv) :-
  add_scope(Env, Name, Scope),
  add_var(Env, ident(Scope, Name) @ Pos, Rhs, RetEnv).
add_var(Env, ident(Scope, Name) @ Pos, Rhs, RetEnv) :-
  \+ Scope = "",
  update_var(Env, Vars, [ident(Scope, Name) @ Pos => Rhs | Vars], RetEnv).

% get_var(+Env, ident(?Scope, ?Name), ?Pos, ?Rhs)
%
% Look up ident variable from env.
% variables and functions have different namespace.
% For example, below code outputs "1", and "42".
%
%		let has = 42
%		echo has("eval")
%		echo has
%
% See call_func/3 for the look-up of functions.
%
get_var(Env, ident(InScope, Name), Pos, Rhs) :-
  ((nonvar(Name), nonvar(InScope), InScope = "") ->
    add_scope(Env, Name, Scope);
    Scope = InScope),
  get_vars(Env, Vars),
  member(ident(Scope, Name) @ Pos => Rhs, Vars).

% add_scope(+Env, +Name, -Scope)
add_scope(Env, Name, Scope) :-
  compat(Name, Scope, _), !;
  get_level(Env, Lv),
  Lv > 0 -> Scope = "l"; Scope = "g".

% get_func(+Env, ident(?Scope, ?Name), ?Pos, ?Func)
get_func(Env, ident(Scope, Name), Pos, Func) :-
  get_funcs(Env, Funcs),
  Func = function(ident(Scope, Name) @ _, _, _),
  member(Func @ Pos, Funcs).

% ===================== Vim script syntax =====================

% eval_expr(+T, -R)
eval_expr(T, R) :- new_env(Env), eval(Env, T, Env, R).
% eval_expr(+Env, +T, -R)
eval_expr(Env, T, R) :- eval(Env, T, Env, R).

% eval_excmds(+Env, +Excmds, -RetEnv)
eval_excmds(Env, Excmds, RetEnv) :-
  foldl(eval_excmd1, Excmds, Env, RetEnv).
eval_excmd1(Excmd, Env, RetEnv) :-
  eval(Env, Excmd, RetEnv, tVoid @ _).

% eval(+Env, +T, -RetEnv, -R)
eval(Env, T, RetEnv, R) :-
  run_hook(Env, on_enter),
  traverse(Env, T, RetEnv, R),
  run_hook(RetEnv, on_leave),
  !.

% Primitive types (with position)
traverse(Env, T @ Pos, Env, R @ Pos) :- prim(T), R = T, !.

% traverse(+Env, file(+Excmds) @ +Pos, -RetEnv, -R)
traverse(Env, file(Body) @ Pos, RetEnv, tVoid @ Pos) :-
  eval_excmds(Env, Body, RetEnv),
  !.

% TODO type comment
% traverse(+Env, comment(+Text) @ +Pos, -Env, -R)
traverse(Env, comment(_) @ Pos, Env, tVoid @ Pos) :- !.

% TODO analyze arguments of excmd
% traverse(+Env, excmd(+Command) @ +Pos, -RetEnv, -R)
traverse(Env, excmd(_) @ Pos, Env, tVoid @ Pos) :- !.

% traverse(+Env, return(+Expr) @ +Pos, -RetEnv, -R)
traverse(Env, return(Expr) @ Pos, Env, tVoid @ Pos) :-
  eval_expr(Env, Expr, _),
  !.

% traverse(+Env, function(+Name, +Params, +Body) @ +Pos, -RetEnv, -R)
traverse(Env, function(Name, Params, Body) @ Pos, RetEnv, tVoid @ Pos) :-
  add_func(Env, function(Name, Params, Body) @ Pos, Env1),
  eval_excmds(Env1, Body, RetEnv),
  !.

% traverse(+Env, excall(+FuncCall) @ +Pos, -Env, -R)
traverse(Env, excall(FuncCall) @ Pos, Env, tVoid @ Pos) :-
  eval_expr(Env, FuncCall, _),
  !.

% traverse(+Env, let(+Lhs, +Op, +Rhs) @ +Pos, -RetEnv, -R)
traverse(Env, let(Lhs, -=, Rhs) @ LetPos, RetEnv, tVoid @ LetPos) :-
  Value = op(Lhs, -, Rhs),
  traverse(Env, let(Lhs, =, Value) @ LetPos, RetEnv, tVoid @ LetPos),
  !.
traverse(Env, let(Lhs, =, Rhs) @ LetPos, RetEnv, tVoid @ LetPos) :-
  (Lhs = ident(Scope, Name) @ IdentPos ->
    add_var(Env, ident(Scope, Name) @ IdentPos, Rhs, RetEnv);
    true),    % TODO subscript, dot, ...
  !.

% traverse(+Env, if(+Cond, +Body) @ +Pos, -RetEnv, -R)
traverse(Env, if(Cond, Body) @ Pos, RetEnv, tVoid @ Pos) :-
  eval_expr(Env, Cond, R @ _),
  to_bool(R, _),
  eval_excmds(Env, Body, RetEnv),
  !.

% TODO
% traverse(+Env, if(+Cond, +Body, else(+ElseBody)) @ +Pos, -RetEnv, -R)
% traverse(+Env, if(+Cond, +Body, elseif([+ElseCond, +ElseIfBody, ...])) @ +Pos, -RetEnv, -R)
% traverse(+Env, if(+Cond, +Body, elseif([+ElseCond, +ElseIfBody, ...]), else(+ElseBody)) @ +Pos, -RetEnv, -R)

% traverse(+Env, echo(+ExprList) @ +Pos, -Env, -R)
traverse(Env, echo(ExprList) @ Pos, Env, tVoid @ Pos) :-
  maplist(eval_expr(Env), ExprList, _),
  !.

% TODO
% traverse(+Env, subscript(+Left, +Right) @ +Pos, -Env, -R)
traverse(Env, subscript(Left @ _, Right @ _) @ Pos, Env, R @ Pos) :-
  eval_expr(Env, Left @ _, Left1 @ _),
  eval_expr(Env, Right @ _, Right1 @ _),
  get_prop(Left1, Right1, R),
  !.

% traverse(+Env, dot(+Left, +Right) @ +Pos, -Env, -R)
traverse(Env, dot(Left @ _, ident(_, Name) @ _) @ Pos, Env, R @ Pos) :-
  eval_expr(Env, Left @ _, Left1 @ _),
  get_prop(Left1, tString(Name), R),
  !.

% TODO
% traverse(+Env, op(+Left, +Op, +Right) @ +Pos, -Env, -R)
traverse(Env, op(_, ==, _) @ Pos, Env, tInt(_) @ Pos) :- !.

% traverse(+Env, call(+Fun, +Args) @ +Pos, -Env, -R)
traverse(Env, call(Fun, Args) @ CallPos, Env, R @ CallPos) :-
  call_func(Env, call(Fun, Args), R),
  !.

% traverse(+Env, ident(+Scope, +Name) @ +Pos, -Env, -R)
traverse(Env, ident(Scope, Name) @ Pos, Env, Rhs @ Pos) :-
  get_var(Env, ident(Scope, Name), _, Rhs @ _),
  !.
