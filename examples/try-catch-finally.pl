:- consult('../lib/vimscript.pl').
:- use_module('../lib/vimscript.pl').

main :- eval(file([
    try([
      echo([tString("try") @ [2,8]]) @ [2,3]
    ]) @ [1,1],
    try([
      echo([tString("try") @ [6,8]]) @ [6,3]
    ],finally([
      echo([tString("finally") @ [8,8]]) @ [8,3]
    ])) @ [5,1],
    try([
      echo([tString("try") @ [12,8]]) @ [12,3]
    ],catch(["",[
      echo([tString("catch") @ [14,8]]) @ [14,3]
    ]])) @ [11,1],
    try([
      echo([tString("try") @ [18,8]]) @ [18,3]
    ],catch(["E478",[
      echo([tString("catch /E478/") @ [20,8]]) @ [20,3]
    ],"",[
      echo([tString("catch //") @ [22,8]]) @ [22,3]
    ],"",[
      echo([tString("catch") @ [24,8]]) @ [24,3]
    ]]),finally([
      echo([tString("finally") @ [26,8]]) @ [26,3]
    ])) @ [17,1]
    ]) @ [1,1], _, _), halt.

:- main; halt(1).

