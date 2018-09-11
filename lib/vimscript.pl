:- module(vimscript, [
  op(500, xfy, ::),
  (::)/2,
  op(600, xfy, @),
  (@)/2,
  op(700, xfy, =>),
  (=>)/2,
  new_eval_env/1,
  eval/3,
  eval_expr/2
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

% update_assoc_list(+Elem, ?List, +Updated, -UpdatedList)
%
% NOTE: Elem must be matched to one or zero elements in List.
% update_assoc_list(N, [1,2,3,1], M, L).
% N = 1, L = [M,2,3,M]
%
update_assoc_list(Elem, List, Updated, UpdatedList) :-
  maplist(update(Elem, Updated), List, UpdatedList).
update(Elem, Updated, Elem, Updated).
update(X, _, Y, Y) :- \+ X = Y.

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
vimfunc(_, "call", [Fun, tList(Args) @ _] :: R @ _) :-
  R = call(Fun, Args),
  !.
% Name must not be a variable, and must be a vimfunc.
vimfunc(_, "function", [tString(Name) @ _] :: R @ _) :-
  var(Name),
  R = _ :: _,
  !.
vimfunc(Env, "function", [tString(Name) @ _] :: FunT @ _) :-
  nonvar(Name),
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

% new_eval_env(-Env)
new_eval_env(Env) :-
  new_env(E1),
  add_hooks(E1, [on_leave:on_let_enter, on_leave:on_function_enter], E2),
  append(E2, [stack:[42], lv:0, vars:[], funcs:[]], Env).

% get_result(+Env, -Result)
get_result(Env, Top) :- member(stack:[Top | _], Env).

% eval(+Env, +Node, -RetEnv, -Result)
eval(Node, RetEnv, Result) :-
  new_eval_env(Env),
  eval(Env, Node, RetEnv, Result).

% eval_expr(+Node, -Result)
eval_expr(Node, Result) :- new_eval_env(Env), eval(Env, Node, Env, Result).

% ----------------- private -----------------

eval(Env, Node, RetEnv, Result) :-
  traverse(Env, Node, RetEnv),
  get_result(RetEnv, Result).

eval_expr(Env, Node, Result) :- eval(Env, Node, Env, Result).

% get_level(+Env, -Lv)
get_level(Env, Lv) :- member(lv:Lv, Env).

% get_vars(+Env, -Vars)
get_vars(Env, Vars) :- member(vars:Vars, Env).

% get_funcs(+Env, -Funcs)
get_funcs(Env, Funcs) :- member(funcs:Funcs, Env).

% update_var(+Env, +Vars, -UpdateVars, -RetEnv)
update_var(Env, Vars, UpdateVars, RetEnv) :-
  update_assoc_list(vars:Vars, Env, UpdateVars, RetEnv).

% update_func(+Env, +Funcs, -UpdateFuncs, -RetEnv)
update_func(Env, Funcs, UpdateFuncs, RetEnv) :-
  update_assoc_list(funcs:Funcs, Env, UpdateFuncs, RetEnv).

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
  eval(Env, ident(Scope, Name) @ _, _, FunT @ _),
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

% get_var(+Env, ident(?Scope, ?Name), -Pos, -Rhs)
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

% ===================== Evaluation functions =====================

% TODO destructuring, subscript, dot, ...
on_let_enter(Env, let(Lhs, Op, Rhs) @ _, _, RetEnv) :-
  add_var(Env, Lhs, [Op, Rhs], RetEnv),
  !.
on_function_enter(Env, function(Name, Params, Body) @ Pos, _, RetEnv) :-
  add_func(Env, function(Name, Params, Body) @ Pos, RetEnv),
  !.

% ===================== Traversal functions =====================

% new_env(-Env)
new_env([hooks:[]]).

% get_hooks(+Env, -Hooks)
get_hooks(Env, Hooks) :- member(hooks:Hooks, Env).
% add_hooks(+Env, +FuncList, -RetEnv)
add_hooks(Env, FuncList, RetEnv) :-
  update_assoc_list(hooks:Hooks, Env, hooks:Added, RetEnv),
  append(FuncList, Hooks, Added).

% run_hook(+Env, +Node, +Event, -RetEnv)
run_hook(Env, Node, Event, RetEnv) :-
  get_hooks(Env, Hooks),
  findall(Func, member(Event:Func, Hooks), FuncList),
  foldl(do_run_hook(Node, Event), FuncList, Env, RetEnv).
do_run_hook(Node, Event, Func, Env, RetEnv) :-
  call(Func, Env, Node, Event, RetEnv); RetEnv = Env.

% traverse(+Env, +Node, -RetEnv)
traverse(Env, Node, RetEnv) :-
  run_hook(Env, Node, on_enter, E1),
  trav(E1, Node, E2),
  run_hook(E2, Node, on_leave, RetEnv).

% traverse_list(+Env, +NodeList, -RetEnv)
traverse_list(Env, NodeList, RetEnv) :- foldl(esrevart, NodeList, Env, RetEnv).
esrevart(Node, Env, RetEnv) :- traverse(Env, Node, RetEnv).

% Primitive types (with position)
trav(Env, Node @ _, Env) :- prim(Node), !.

% trav(+Env, file(+Excmds) @ +Pos, -RetEnv)
trav(Env, file(Body) @ _, RetEnv) :- traverse_list(Env, Body, RetEnv), !.

% TODO type comment
% trav(+Env, comment(+Text) @ +Pos, -RetEnv)
trav(Env, comment(_) @ _, Env) :- !.

% TODO analyze arguments of excmd
% trav(+Env, excmd(+Command) @ +Pos, -RetEnv)
trav(Env, excmd(_) @ _, Env) :- !.

% trav(+Env, return(+Expr) @ +Pos, -RetEnv)
trav(Env, return(Expr) @ _, RetEnv) :- traverse(Env, Expr, RetEnv), !.

% trav(+Env, function(+Name, +Params, +Body) @ +Pos, -RetEnv)
trav(Env, function(Name, Params, Body) @ _, RetEnv) :-
  append([Name | Params], Body, L),
  traverse_list(Env, L, RetEnv),
  !.

% trav(+Env, excall(+FuncCall) @ +Pos, -RetEnv)
trav(Env, excall(FuncCall) @ _, RetEnv) :- traverse(Env, FuncCall, RetEnv), !.

% trav(+Env, let(+Lhs, +Op, +Rhs) @ +Pos, -RetEnv)
trav(Env, let(Lhs, _, Rhs) @ _, RetEnv) :- traverse_list(Env, [Lhs, Rhs], RetEnv), !.

% trav(+Env, if(+Cond, +Body) @ +Pos, -RetEnv)
trav(Env, if(Cond, Body) @ _, RetEnv) :- traverse_list(Env, [Cond | Body], RetEnv), !.

% TODO
% trav(+Env, if(+Cond, +Body, else(+ElseBody)) @ +Pos, -RetEnv)
% trav(+Env, if(+Cond, +Body, elseif([+ElseCond, +ElseIfBody, ...])) @ +Pos, -RetEnv)
% trav(+Env, if(+Cond, +Body, elseif([+ElseCond, +ElseIfBody, ...]), else(+ElseBody)) @ +Pos, -RetEnv)

% trav(+Env, echo(+ExprList) @ +Pos, -RetEnv)
trav(Env, echo(ExprList) @ _, RetEnv) :- traverse_list(Env, ExprList, RetEnv), !.

% TODO
% trav(+Env, subscript(+Left, +Right) @ +Pos, -RetEnv)
trav(Env, subscript(Left, Right) @ _, RetEnv) :- traverse_list(Env, [Left, Right], RetEnv), !.

% trav(+Env, dot(+Left, +Right) @ +Pos, -RetEnv)
trav(Env, dot(Left, Right) @ _, RetEnv) :- traverse_list(Env, [Left, Right], RetEnv), !.

% trav(+Env, op(+Left, +Op, +Right) @ +Pos, -RetEnv)
trav(Env, op(Left, _, Right) @ _, RetEnv) :- traverse_list(Env, [Left, Right], RetEnv), !.

% trav(+Env, call(+Fun, +Args) @ +Pos, -RetEnv)
trav(Env, call(Fun, Args) @ _, RetEnv) :- traverse_list(Env, [Fun | Args], RetEnv), !.

% trav(+Env, ident(+Scope, +Name) @ +Pos, -RetEnv)
trav(Env, ident(_, _) @ _, Env) :- !.
