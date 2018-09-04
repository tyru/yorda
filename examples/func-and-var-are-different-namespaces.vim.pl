file([
  let(ident("g","has",0) @ [1,5],=,tInt(42) @ [1,11]) @ [1,1],
  echo([call(ident("g","has",0) @ [2,6],[tString("eval") @ [2,10]]) @ [2,9]]) @ [2,1],
  echo([ident("g","has",0) @ [3,6]]) @ [3,1]
]) @ [1,1]
