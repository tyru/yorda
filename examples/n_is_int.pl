new_env(Env), eval(Env, [
file([
  let(ident("","n") @ [1,5],=,tInt(42) @ [1,9]) @ [1,1],
  echo([ident("","n") @ [2,6]]) @ [2,1]
])
], RetEnv, R). @ [1,1]
