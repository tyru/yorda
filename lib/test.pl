:- consult(vimscript).
:- use_module(vimscript).

:- discontiguous test/1, test/2.

:- begin_tests(syntax_error).
  test(no_such_term, [fail]) :- eval(foobar, _, _).
:- end_tests(syntax_error).

:- begin_tests(primitive_types).
  test(tAny, [true(R = tAny @ [1, 1])]) :-
    eval(tAny @ [1, 1], _, R).
  test(tVoid, [true(R = tVoid @ [1, 1])]) :-
    eval(tVoid @ [1, 1], _, R).
  test(tInt, [true(R = tInt(_) @ [1, 1])]) :-
    eval(tInt(_) @ [1, 1], _, R).
  test(tFloat, [true(R = tFloat(_) @ [1, 1])]) :-
    eval(tFloat(_) @ [1, 1], _, R).
  test(tString, [true(R = tString(_) @ [1, 1])]) :-
    eval(tString(_) @ [1, 1], _, R).
  test(tList, [true(R = tList(_) @ [1, 1])]) :-
    eval(tList(_) @ [1, 1], _, R).
  test(tDict, [true(R = tDict(_) @ [1, 1])]) :-
    eval(tDict(_) @ [1, 1], _, R).
  test(tTuple0, [true(R = tTuple([]) @ [1, 1])]) :-
    eval(tTuple([]) @ [1, 1], _, R).
  test(tTuple1, [true(R = tTuple([tInt(42) @ [1, 2]]) @ [1, 1])]) :-
    eval(tTuple([tInt(42) @ [1, 2]]) @ [1, 1], _, R).
  test(tTuple2, [true(R = tTuple([tInt(42) @ [1, 2], tFloat(12.34) @ [1, 3]]) @ [1, 1])]) :-
    eval(tTuple([tInt(42) @ [1, 2], tFloat(12.34) @ [1, 3]]) @ [1, 1], _, R).

  test(non_string_dict_key, [true(R = tError('key is not string')@[1, 2])]) :-
    eval(tDict([tInt(42) @ [1,2] : tInt(42) @ [1,3]]) @ [1,1], _, R).
:- end_tests(primitive_types).

:- begin_tests(expr).
  % $ENV is string
  test(env_is_string, [true(R = tString(_) @ [1,1])]) :-
    eval(env("ENV") @ [1,1], _, R).
  % @a is string
  test(reg_is_string, [true(R = tString(_) @ [1,1])]) :-
    eval(reg("a") @ [1,1], _, R).
  % &expandtab
  test(option_expandtab, [true(R = tBool(_) @ [1,1])]) :-
    eval(option("expandtab") @ [1,1], _, R).
  % &et
  test(option_et, [true(R = tBool(_) @ [1,1])]) :-
    eval(option("et") @ [1,1], _, R).
  % &unknown
  test(option_unknown, [true(R = tError('no such option: unknown') @ [1,1])]) :-
    eval(option("unknown") @ [1,1], _, R).
  % abs(42) == 42
  test(call_abs_int, [true(R = tInt(42) @ [1,1])]) :-
    Expr = call(ident("","abs") @ nopos, [tInt(42) @ [1,1]]) @ [1,1],
    eval(Expr, _, R).
  % abs(12.34) == 12.34
  test(call_abs_float, [true(R = tFloat(12.34) @ [1,1])]) :-
    Expr = call(ident("","abs") @ nopos, [tFloat(12.34) @ [1,1]]) @ [1,1],
    eval(Expr, _, R).
  % abs(<tInt>) != <tFloat>
  test(call_abs_fail1, [fail]) :-
    Expr = call(ident("","abs") @ nopos, [tInt(42) @ [1,1]]) @ [1,1],
    eval(Expr, _, R),
    R = tFloat(_) @ [1,1].
  % abs(<tFloat>) != <tInt>
  test(call_abs_fail2, [fail]) :-
    Expr = call(ident("","abs") @ nopos, [tFloat(12.34) @ [1,1]]) @ [1,1],
    eval(Expr, _, R),
    R = tInt(_) @ [1,1].
  % acos(0.0) == 1.570796
  test(call_acos, [true(R = tFloat(_) @ [1,1])]) :-
    Expr = call(ident("","acos") @ nopos, [tFloat(0) @ [1,1]]) @ [1,1],
    eval(Expr, _, R).
  % add([], 42) == [42]
  test(call_add, [true(R = tList([tInt(42) @ [1,5]]) @ [1,1])]) :-
    Expr = call(ident("","add") @ nopos, [tList([]) @ [1,1], tInt(42) @ [1,5]]) @ [1,1],
    eval(Expr, _, R).
  % and(1, 1) == 1
  test(call_and, [true(R = tInt(1) @ [1,1])]) :-
    Expr = call(ident("","and") @ nopos, [tInt(1) @ nopos, tInt(1) @ [1,1]]) @ [1,1],
    eval(Expr, _, R).
  % append(1, "foo") == 0
  test(call_append, [true(R = tInt(0) @ [1,1])]) :-
    Expr = call(ident("","append") @ nopos, [tInt(1) @ nopos, tString("foo") @ nopos]) @ [1,1],
    eval(Expr, _, R).
  % append(".", "foo") == 0
  test(call_append, [true(R = tInt(0) @ [1,1])]) :-
    Expr = call(ident("","append") @ nopos, [tString(".") @ nopos, tString("foo") @ nopos]) @ [1,1],
    eval(Expr, _, R).
  % append(1, []) == 0
  test(call_append, [true(R = tInt(0) @ [1,1])]) :-
    Expr = call(ident("","append") @ nopos, [tInt(1) @ nopos, tList([]) @ nopos]) @ [1,1],
    eval(Expr, _, R).
  % append(".", []) == 0
  test(call_append, [true(R = tInt(0) @ [1,1])]) :-
    Expr = call(ident("","append") @ nopos, [tString(".") @ nopos, tList([]) @ nopos]) @ [1,1],
    eval(Expr, _, R).
  % has("eval") == 1
  test(call_has, [true(R = tInt(1) @ [1,1])]) :-
    Expr = call(ident("","has") @ nopos, [tString("eval") @ nopos]) @ [1,1],
    eval(Expr, _, R).
:- end_tests(expr).

:- begin_tests(node).
  % file(Excmds)
  test(file1, [true(R = tSuccess)]) :-
    eval(file([]) @ [1,1], _, R).
  test(file2, [true(R = tSuccess)]) :-
    eval(file([comment("comment") @ nopos]) @ [1,1], _, R).

  % comment(Text)
  test(comment, [true(R = tSuccess)]) :-
    eval(comment("this is a comment") @ [1,1], _, R).
:- end_tests(node).

:- begin_tests(excmds).
  % let n = 42
  % echo n
  test(n_is_int, [true(Results = [(ident("g", "n"), Pos, Rhs)])]) :-
    Var = ident("", "n"),
    Pos = [1, 5],
    Rhs = tInt(42) @ [1, 9],
    Results = [(ident("g", "n"), Pos, Rhs)],
    eval(file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([Var @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tSuccess),
    findall((X, Y, Z), vimscript:get_var(RetEnv, X, Y, Z), Results).

  % let has = 42
  % echo has("eval")
  % echo has
  test(func_and_var_are_different_namespaces) :-
    Var = ident("", "has"),
    Pos = [1, 5],
    Rhs = tInt(42) @ [1, 11],
    Results = [(ident("g", "has"), Pos, Rhs)],
    eval(file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(ident("","has") @ nopos, [tString("eval") @ nopos]) @ nopos]) @ nopos,
      echo([Var @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tSuccess),
    findall((X, Y, Z), vimscript:get_var(RetEnv, X, Y, Z), Results).

  % echo function('has')('eval')
  test(funcref1) :-
    eval(file([
      echo([call(call(ident("","function") @ nopos,[tString("has") @ nopos]) @ nopos,[tString("eval") @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], _, tSuccess).

  % let F = function('has')
  % echo F('eval')
  test(funcref2) :-
    Var = ident("", "F"),
    Pos = [3, 5],
    Rhs = call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17],
    Results = [(ident("g", "F"), Pos, Rhs)],
    eval(file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(Var @ nopos, [tString("eval") @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tSuccess),
    findall((X, Y, Z), vimscript:get_var(RetEnv, X, Y, Z), Results).

  % let g:F = function('has')
  % echo g:F('eval')
  test(funcref3) :-
    Var = ident("g", "F"),
    Pos = [3, 5],
    Rhs = call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17],
    Results = [(Var, Pos, Rhs)],
    eval(file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(Var @ nopos, [tString("eval") @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tSuccess),
    findall((X, Y, Z), vimscript:get_var(RetEnv, X, Y, Z), Results).

  % echo call(function('has'), ['eval'])
  test(call1) :-
    eval(file([
      echo([
        call(
          ident("","call") @ [1,6],
          [
            call(ident("","function") @ [1,11],[tString("has") @ [1,20]]) @ [1,19],
            tList([tString("eval") @ [1,29]]) @ [1,28]
          ]
        ) @ [1,10]
      ]) @ [1,1]
    ]) @ [1,1], _, tSuccess).

  % let F = function('has')
  % echo call(F, ['eval'])
  test(call2) :-
    Var = ident("", "F"),
    Pos = [3, 5],
    Rhs = call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17],
    Results = [(ident("g", "F"), Pos, Rhs)],
    eval(file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(ident("","call") @ nopos,[Var @ nopos,tList([tString("eval") @ nopos]) @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tSuccess),
    findall((X, Y, Z), vimscript:get_var(RetEnv, X, Y, Z), Results).

  % let g:F = function('has')
  % echo call(g:F, ['eval'])
  test(call3) :-
    Var = ident("g", "F"),
    Pos = [3, 5],
    Rhs = call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17],
    Results = [(Var, Pos, Rhs)],
    eval(file([
      let(Var @ Pos, =, Rhs) @ nopos,
      echo([call(ident("","call") @ nopos, [Var @ nopos, tList([tString("eval") @ nopos]) @ nopos]) @ nopos]) @ nopos
    ]) @ [1,1], RetEnv, tSuccess),
    findall((X, Y, Z), vimscript:get_var(RetEnv, X, Y, Z), Results).

  % echo call(function('has'), ['eval'], {})
  test(call1) :-
    eval(file([
      echo([
        call(
          ident("","call") @ [1,6],
          [
            call(ident("","function") @ [1,11],[tString("has") @ [1,20]]) @ [1,19],
            tList([tString("eval") @ [1,29]]) @ [1,28],
            tDict([]) @ [1,40]
          ]
        ) @ [1,10]
      ]) @ [1,1]
    ]) @ [1,1], _, tSuccess).


  test(define_func) :-
    FuncName = ident("s","id"),
    Func = function(FuncName @ nopos,[ident("","x") @ [1,16]],[
      return(ident("a","x") @ nopos) @ nopos
    ]),
    Pos = [1, 2],
    Results = [(FuncName, Pos, Func)],
    eval(file([Func @ Pos]) @ [1,1], RetEnv, tSuccess),
    findall((X, Y, Z), vimscript:get_func(RetEnv, X, Y, Z), Results).

:- end_tests(excmds).

:- run_tests.
:- halt.
