file([
  echo([call(call(ident("","function") @ [1,6],[tString("has") @ [1,15]]) @ [1,14],[tString("eval") @ [1,22]]) @ [1,21]]) @ [1,1],
  let(ident("","F") @ [3,5],=,call(ident("","function") @ [3,9],[tString("has") @ [3,18]]) @ [3,17]) @ [3,1],
  echo([call(ident("","F") @ [4,6],[tString("eval") @ [4,8]]) @ [4,7]]) @ [4,1],
  let(ident("g","F") @ [6,5],=,call(ident("","function") @ [6,11],[tString("has") @ [6,20]]) @ [6,19]) @ [6,1],
  echo([call(ident("g","F") @ [7,6],[tString("eval") @ [7,10]]) @ [7,9]]) @ [7,1]
]) @ [1,1]
