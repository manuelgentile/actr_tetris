# The role of mental rotation in TetrisTM gameplay: an ACT-R computational cognitive model

This repo contains the ACT-R model described in the the paper "The role of mental rotation in Tetris<sup>TM</sup> gameplay: an ACT-R computational cognitive model".

The provided computational cognitive model is based on the ACT-R cognitive architecture (http://act-r.psy.cmu.edu/), and show how it is possibile to re-design the game of #Tetris for educational purposes in order to improve the mental rotation ability of the players.

Mental rotation is an essential spatial reasoning skill in human cognition and has proven to be an essential predictor of mathematical and STEM skills. 


# How to run the code

The first step is to download the source code version of ACT-R from the website http://act-r.psy.cmu.edu/ .
Then a folder 'models/mental' should be create in the root of the unzipped folder and all the code should be copied inside.
To run and load ACT-R:

``` 
cd actr7.x
rlwrap sbcl --load load-act-r.lisp 
``` 

Then, run this line of code in the ACT-R terminal to load the model. 

``` lisp
(actr-load "ACT-R:models;mental;mental-jigsaw.lisp")
```

The 'runsim' function allows the user to run the model on the provided data.

``` lisp
(runsim 1 2 4 4)
```

# Acknowledgement

The model uses the spatial-imagery functions provided by David Peebles in the imagery-functions.lisp file (to a complete description see 
https://github.com/djpeebles/act-r-mental-rotation-models)
