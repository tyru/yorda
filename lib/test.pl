:- consult(vimscript).
:- use_module(vimscript).

:- begin_tests(error).
  test(no_such_term) :- not(eval_expr(foobar, _)).
  test(not_equal) :- not(eval_expr(tInt(42), tInt(999))).
:- end_tests(error).

:- begin_tests(primitive_types).
  test(tAny, all(R = [tAny @ Pos])) :-
    eval_expr(tAny @ Pos, R).
  test(tVoid, all(R = [tVoid @ Pos])) :-
    eval_expr(tVoid @ Pos, R).
  test(tInt, all(R = [tInt(_) @ Pos])) :-
    eval_expr(tInt(_) @ Pos, R).
  test(tFloat, all(R = [tFloat(_) @ Pos])) :-
    eval_expr(tFloat(_) @ Pos, R).
  test(tString, all(R = [tString(_) @ Pos])) :-
    eval_expr(tString(_) @ Pos, R).
  test(tList, all(R = [tList(_) @ Pos])) :-
    eval_expr(tList(_) @ Pos, R).
  test(tDict, all(R = [tDict(_) @ Pos])) :-
    eval_expr(tDict(_) @ Pos, R).
  test(tTuple0, all(R = [tTuple @ Pos])) :-
    eval_expr(tTuple @ Pos, R).
  test(tTuple1, all(R = [tTuple(_) @ Pos])) :-
    eval_expr(tTuple(_) @ Pos, R).
  test(tTuple2, all(R = [tTuple(_, _) @ Pos])) :-
    eval_expr(tTuple(_, _) @ Pos, R).
  test(tTuple3, all(R = [tTuple(_, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _) @ Pos, R).
  test(tTuple4, all(R = [tTuple(_, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _) @ Pos, R).
  test(tTuple5, all(R = [tTuple(_, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _) @ Pos, R).
  test(tTuple6, all(R = [tTuple(_, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _) @ Pos, R).
  test(tTuple7, all(R = [tTuple(_, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _) @ Pos, R).
  test(tTuple8, all(R = [tTuple(_, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple9, all(R = [tTuple(_, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple10, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple11, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple12, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple13, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple14, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple15, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple16, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple17, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple18, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple19, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
  test(tTuple20, all(R = [tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos])) :-
    eval_expr(tTuple(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) @ Pos, R).
:- end_tests(primitive_types).

:- begin_tests(expr).
  % abs(42) == 42
  test(call_abs_int) :-
    eval_expr(
      call(ident("","abs") @ nopos, [tInt(42) @ nopos]) @ [1,1],
      tInt(42) @ [1,1]).
  % abs(12.34) == 12.34
  test(call_abs_float) :-
    eval_expr(
      call(ident("","abs") @ nopos, [tFloat(12.34) @ nopos]) @ [1,1],
      tFloat(12.34) @ [1,1]).
  % abs(<tInt>) != <tFloat>
  test(call_abs_fail1) :-
    not(eval_expr(
      call(ident("","abs") @ nopos, [tInt(42) @ nopos]) @ [1,1],
      tFloat(_) @ [1,1])).
  % abs(<tFloat>) != <tInt>
  test(call_abs_fail2) :-
    not(eval_expr(
      call(ident("","abs") @ nopos, [tFloat(12.34) @ nopos]) @ [1,1],
      tInt(_) @ [1,1])).
  % acos(0.0) == 1.570796
  test(call_acos) :-
    eval_expr(
      call(ident("","acos") @ nopos, [tFloat(0) @ nopos]) @ [1,1],
      tFloat(1.570796) @ [1,1]).
  % add([], 42) == [42]
  test(call_add) :-
    eval_expr(
      call(ident("","add") @ nopos, [tList([]) @ nopos, tInt(42) @ [1,5]]) @ [1,1],
      tList([tInt(42) @ [1,5]]) @ [1,1]).
  % and(1, 1) == 1
  test(call_and) :-
    eval_expr(
      call(ident("","and") @ nopos, [tInt(1) @ nopos, tInt(1) @ nopos]) @ [1,1],
      tInt(1) @ [1,1]).
  % append(1, "foo") == 0
  test(call_append) :-
    eval_expr(
      call(ident("","append") @ nopos, [tInt(1) @ nopos, tString("foo") @ nopos]) @ [1,1],
      tInt(0) @ [1,1]).
  % append(".", "foo") == 0
  test(call_append) :-
    eval_expr(
      call(ident("","append") @ nopos, [tString(".") @ nopos, tString("foo") @ nopos]) @ [1,1],
      tInt(0) @ [1,1]).
  % append(1, []) == 0
  test(call_append) :-
    eval_expr(
      call(ident("","append") @ nopos, [tInt(1) @ nopos, tList([]) @ nopos]) @ [1,1],
      tInt(0) @ [1,1]).
  % append(".", []) == 0
  test(call_append) :-
    eval_expr(
      call(ident("","append") @ nopos, [tString(".") @ nopos, tList([]) @ nopos]) @ [1,1],
      tInt(0) @ [1,1]).
  % has("eval") == 1
  test(call_has) :-
    eval_expr(
      call(ident("","has") @ nopos, [tString("eval") @ nopos]) @ [1,1],
      tInt(1) @ [1,1]).
:- end_tests(expr).

:- begin_tests(node).
  % file(Excmds)
  test(file1, all(R = [tVoid @ [1,1]])) :-
    eval_expr(file([]) @ [1,1], R).
  test(file2, all(R = [tVoid @ [1,1]])) :-
    eval_expr(file([comment("comment") @ nopos]) @ [1,1], R).

  % comment(Text)
  test(comment, all(R = [tVoid @ [1,1]])) :-
    eval_expr(comment("this is a comment") @ [1,1], R).
:- end_tests(node).

:- begin_tests(excmds).
  % let n = 42
  % echo n
  test(n_is_int) :-
    Var = ident("", "n"),
    Pos = [1, 5],
    Rhs = tInt(42) @ [1, 9],
    Results = [(ident("g", "n"), Pos, Rhs)],
    new_env(Env),
    eval(Env, file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([Var @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tVoid @ [1,1]),
    findall((X, Y, Z), get_var(RetEnv, X, Y, Z), Results).

  % let has = 42
  % echo has("eval")
  % echo has
  test(func_and_var_are_different_namespaces) :-
    Var = ident("", "has"),
    Pos = [1, 5],
    Rhs = tInt(42) @ [1, 11],
    Results = [(ident("g", "has"), Pos, Rhs)],
    new_env(Env),
    eval(Env, file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(ident("","has") @ nopos, [tString("eval") @ nopos]) @ nopos]) @ nopos,
      echo([Var @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tVoid @ [1,1]),
    findall((X, Y, Z), get_var(RetEnv, X, Y, Z), Results).

  % echo function('has')('eval')
  test(funcref1) :-
    eval_expr(file([
      echo([call(call(ident("","function") @ nopos,[tString("has") @ nopos]) @ nopos,[tString("eval") @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], tVoid @ [1,1]).

  % let F = function('has')
  % echo F('eval')
  test(funcref2) :-
    Var = ident("", "F"),
    Pos = [3, 5],
    Rhs = call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17],
    Results = [(ident("g", "F"), Pos, Rhs)],
    new_env(Env),
    eval(Env, file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(Var @ nopos, [tString("eval") @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tVoid @ [1,1]),
    findall((X, Y, Z), get_var(RetEnv, X, Y, Z), Results).

  % let g:F = function('has')
  % echo g:F('eval')
  test(funcref3) :-
    Var = ident("g", "F"),
    Pos = [3, 5],
    Rhs = call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17],
    Results = [(Var, Pos, Rhs)],
    new_env(Env),
    eval(Env, file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(Var @ nopos, [tString("eval") @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tVoid @ [1,1]),
    findall((X, Y, Z), get_var(RetEnv, X, Y, Z), Results).

  % echo call(function('has'), ['eval'])
  test(call1) :-
    eval_expr(file([
      echo([
        call(
          ident("","call") @ [1,6],
          [
            call(ident("","function") @ [1,11],[tString("has") @ [1,20]]) @ [1,19],
            tList([tString("eval") @ [1,29]]) @ [1,28]
          ]
        ) @ [1,10]
      ]) @ [1,1]
    ]) @ [1,1], tVoid @ [1,1]).

  % let F = function('has')
  % echo call(F, ['eval'])
  test(call2) :-
    Var = ident("", "F"),
    Pos = [3, 5],
    Rhs = call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17],
    Results = [(ident("g", "F"), Pos, Rhs)],
    new_env(Env),
    eval(Env, file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(ident("","call") @ nopos,[Var @ nopos,tList([tString("eval") @ nopos]) @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tVoid @ [1,1]),
    findall((X, Y, Z), get_var(RetEnv, X, Y, Z), Results).

  % let g:F = function('has')
  % echo call(g:F, ['eval'])
  test(call3) :-
    Var = ident("g", "F"),
    Pos = [3, 5],
    Rhs = call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17],
    Results = [(Var, Pos, Rhs)],
    new_env(Env),
    eval(Env, file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(ident("","call") @ nopos, [Var @ nopos, tList([tString("eval") @ nopos]) @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tVoid @ [1,1]),
    findall((X, Y, Z), get_var(RetEnv, X, Y, Z), Results).

:- end_tests(excmds).

:- run_tests.
:- halt.
