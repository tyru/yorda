file([
  let(ident("","has") @ [1,5],=,tInt(42) @ [1,11]) @ [1,1],
  echo([call(ident("","has") @ [2,6],[tString("eval") @ [2,10]]) @ [2,9]]) @ [2,1],
  echo([ident("","has") @ [3,6]]) @ [3,1]
]) @ [1,1]
