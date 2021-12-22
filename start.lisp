cd actr7.x
rlwrap sbcl --load load-act-r.lisp 
(actr-load "ACT-R:models;mental;mental-jigsaw.lisp")
(runsim 1 2 4 4)