file([
  comment(" @param {Dict} wg") @ [1,1],
  comment(" @param {Int} index") @ [2,1],
  comment(" @returns {tInt}") @ [3,1],
  function(ident("s","_notify_done",0) @ [4,11],[ident("","wg",0) @ [4,26],ident("","index",0) @ [4,30],ident("","value",0) @ [4,37]],[
    let(subscript(dot(ident("a","wg",0) @ [5,7],ident("","results",0) @ [5,12]) @ [5,11],ident("a","index",0) @ [5,20]) @ [5,19],=,ident("a","value",0) @ [5,31]) @ [5,3],
    let(dot(ident("a","wg",0) @ [6,7],ident("","remaining",0) @ [6,12]) @ [6,11],=,tInt(1) @ [6,25]) @ [6,3],
    if((dot(ident("a","wg",0) @ [7,6],ident("","remaining",0) @ [7,11]) @ [7,10] == tInt(0) @ [7,24]) @ [7,21],[
      excall(call(dot(ident("a","wg",0) @ [8,10],ident("","resolve",0) @ [8,15]) @ [8,14],[dot(ident("a","wg",0) @ [8,23],ident("","results",0) @ [8,28]) @ [8,27]]) @ [8,22]) @ [8,5]
    ]) @ [7,3]
  ]) @ [4,1]
]) @ [1,1]
