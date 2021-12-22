# actr_tetris

This repo contains the ACT-R model described in the the paper "The role of mental rotation in TetrisTM gameplay: an ACT-R computational cognitive model".

The provided computational cognitive model is based on the ACT-R cognitive architecture (http://act-r.psy.cmu.edu/), and show how it is possibile to re-design the game of #Tetris for educational purposes in order to improve the mental rotation ability of the players.

Mental rotation is an essential spatial reasoning skill in human cognition and has proven to be an essential predictor of mathematical and STEM skills. 


# Run the code

``` lisp
cd actr7.x
rlwrap sbcl --load load-act-r.lisp 
(actr-load "ACT-R:models;mental;mental-jigsaw.lisp")
(runsim 1 2 4 4)
```
