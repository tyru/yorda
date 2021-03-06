:- consult(vimscript).
:- use_module(vimscript).

:- begin_tests(syntax_error).
  test(no_such_term) :- \+ eval(foobar, _, _).
:- end_tests(syntax_error).

:- begin_tests(primitive_types).
  test(tAny) :-
    Value = tAny @ [1, 1],
    eval(Value, _, Value).
  test(tVoid) :-
    Value = tVoid @ [1, 1],
    eval(Value, _, Value).
  test(tInt) :-
    Value = tInt(_) @ [1, 1],
    eval(Value, _, Value).
  test(tFloat) :-
    Value = tFloat(_) @ [1, 1],
    eval(Value, _, Value).
  test(tString) :-
    Value = tString(_) @ [1, 1],
    eval(Value, _, Value).
  test(tList) :-
    Value = tList(_) @ [1, 1],
    eval(Value, _, Value).
  test(tDict) :-
    Value = tDict(_) @ [1, 1],
    eval(Value, _, Value).
  test(tTuple0) :-
    Value = tTuple([]) @ [1, 1],
    eval(Value, _, Value).
  test(tTuple1) :-
    Value = tTuple([tInt(42) @ [1, 2]]) @ [1, 1],
    eval(Value, _, Value).
  test(tTuple2) :-
    Value = tTuple([tInt(42) @ [1, 2], tFloat(12.34) @ [1, 3]]) @ [1, 1],
    eval(Value, _, Value).

  test(not_equal) :-
    eval(tInt(42) @ nopos, _, R), \+ R = tInt(999) @ nopos.
  test(non_string_dict_key) :-
    eval(tDict([tInt(42) @ [1,2] : tInt(42) @ [1,3]]) @ [1,1], _, R),
    R = tError('key is not string')@[1, 2].
:- end_tests(primitive_types).

:- begin_tests(expr).
  % $ENV is string
  test(env_is_string) :-
    eval(
      env("ENV") @ [1,1],
      _,
      tString(_) @ [1,1]).
  % @a is string
  test(reg_is_string) :-
    eval(
      reg("a") @ [1,1],
      _,
      tString(_) @ [1,1]).
  % &expandtab
  test(option_expandtab) :-
    eval(
      option("expandtab") @ [1,1],
      _,
      tInt(_) @ [1,1]).
  % &et
  test(option_et) :-
    eval(
      option("et") @ [1,1],
      _,
      tInt(_) @ [1,1]).
  % &unknown
  test(option_unknown) :-
    eval(option("unknown") @ [1,1], _, R),
    R = tError('no such option: unknown') @ [1,1].
  % abs(42) == 42
  test(call_abs_int) :-
    eval(
      call(ident("","abs") @ nopos, [tInt(42) @ nopos]) @ [1,1],
      _,
      tInt(42) @ [1,1]).
  % abs(12.34) == 12.34
  test(call_abs_float) :-
    eval(
      call(ident("","abs") @ nopos, [tFloat(12.34) @ nopos]) @ [1,1],
      _,
      tFloat(12.34) @ [1,1]).
  % abs(<tInt>) != <tFloat>
  test(call_abs_fail1) :-
    eval(
      call(ident("","abs") @ nopos, [tInt(42) @ nopos]) @ [1,1],
      _,
      R),
    \+ R = tFloat(_) @ [1,1].
  % abs(<tFloat>) != <tInt>
  test(call_abs_fail2) :-
    eval(
      call(ident("","abs") @ nopos, [tFloat(12.34) @ nopos]) @ [1,1],
      _,
      R),
    \+ R = tInt(_) @ [1,1].
  % acos(0.0) == 1.570796
  test(call_acos) :-
    eval(
      call(ident("","acos") @ nopos, [tFloat(0) @ nopos]) @ [1,1],
      _,
      tFloat(1.570796) @ [1,1]).
  % add([], 42) == [42]
  test(call_add) :-
    eval(
      call(ident("","add") @ nopos, [tList([]) @ nopos, tInt(42) @ [1,5]]) @ [1,1],
      _,
      tList([tInt(42) @ [1,5]]) @ [1,1]).
  % and(1, 1) == 1
  test(call_and) :-
    eval(
      call(ident("","and") @ nopos, [tInt(1) @ nopos, tInt(1) @ nopos]) @ [1,1],
      _,
      tInt(1) @ [1,1]).
  % append(1, "foo") == 0
  test(call_append) :-
    eval(
      call(ident("","append") @ nopos, [tInt(1) @ nopos, tString("foo") @ nopos]) @ [1,1],
      _,
      tInt(0) @ [1,1]).
  % append(".", "foo") == 0
  test(call_append) :-
    eval(
      call(ident("","append") @ nopos, [tString(".") @ nopos, tString("foo") @ nopos]) @ [1,1],
      _,
      tInt(0) @ [1,1]).
  % append(1, []) == 0
  test(call_append) :-
    eval(
      call(ident("","append") @ nopos, [tInt(1) @ nopos, tList([]) @ nopos]) @ [1,1],
      _,
      tInt(0) @ [1,1]).
  % append(".", []) == 0
  test(call_append) :-
    eval(
      call(ident("","append") @ nopos, [tString(".") @ nopos, tList([]) @ nopos]) @ [1,1],
      _,
      tInt(0) @ [1,1]).
  % has("eval") == 1
  test(call_has) :-
    eval(
      call(ident("","has") @ nopos, [tString("eval") @ nopos]) @ [1,1],
      _,
      tInt(1) @ [1,1]).
:- end_tests(expr).

:- begin_tests(node).
  % file(Excmds)
  test(file1, all(R = [tSuccess])) :-
    eval(file([]) @ [1,1], _, R).
  test(file2, all(R = [tSuccess])) :-
    eval(file([comment("comment") @ nopos]) @ [1,1], _, R).

  % comment(Text)
  test(comment, all(R = [tSuccess])) :-
    eval(comment("this is a comment") @ [1,1], _, R).
:- end_tests(node).

:- begin_tests(excmds).
  % let n = 42
  % echo n
  test(n_is_int) :-
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
