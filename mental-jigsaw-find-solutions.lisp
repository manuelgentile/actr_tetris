(defun isCompatible(path zoid debug)
    (let 
        (
            (v (max_consecutive_dim path 0))
            (o (max_consecutive_dim path 1))
            (horizontal-span (span path 0))
            (vertical-span (span path 1))
            (vertical-mean (mean_x_dim path 0))
            (horizontal-mean (mean_x_dim path 1))
        )
        (if debug (print (format NIL "~a ~a ~a ~a ~a ~a"  v o vertical-span horizontal-span vertical-mean horizontal-mean)))
        (if (or (char= zoid #\J) (char= zoid #\L))
            (if (/= 6 (* vertical-span horizontal-span))
                (return-from isCompatible NIL)            
                (if (not (or (= v 3) (= o 3)))
                    (return-from isCompatible NIL)
                    (if (not (or (and (= 3 o) (not (integerp vertical-mean)))
                           (and (= 3 v) (not (integerp horizontal-mean)))))
                        (return-from isCompatible NIL)
                        (return-from isCompatible T)
                    )
                )
            )
        )
        
        (if (or (char= zoid #\S) (char= zoid #\Z))
            (if (/= 6 (* vertical-span horizontal-span))
                (return-from isCompatible NIL)
                (return-from isCompatible (and (= 2 v) (= 2 o)))
            )
        )
        
        (if  (char= zoid #\T) 
            (let ()
                
            (if (/= 6 (* vertical-span horizontal-span))
                (return-from isCompatible NIL)          
                (if (not (or (= v 3) (= o 3)))
                    (return-from isCompatible NIL)
                    (if (not (or (and (= 3 o) (integerp vertical-mean))
                           (and (= 3 v) (integerp horizontal-mean))))
                        (return-from isCompatible NIL)
                        (return-from isCompatible T)
                    )
                )
            )
            )

        )
        (if  (char= zoid #\O) 
            (return-from isCompatible (and (= 2 vertical-span) (= 2 horizontal-span)))
        )
        (if  (char= zoid #\I) 
            (if (not (or (and (= vertical-span 1) (= horizontal-span 4))
                         (and (= vertical-span 1) (= horizontal-span 4))))
                (return-from isCompatible NIL)
                (return-from isCompatible (or (= v 4) (= o 4)))
            )

        )
        T
    )
)

(defun isAdherent(port path)
    (let* (
            (min_col (min_x_dim path 1))
            (max_col (max_x_dim path 1))
            (confine (loop for i from min_col to max_col 
                collect (reduce #'max 
                    (mapcar (lambda (y) (nth 0 y)) 
                        (remove-if-not (lambda (x) (= (nth 1 x) i)) path)
                    )
                )
            ))
        )
        (loop for c in confine
            for i from 0
            do
                (if (> (+ c 1) 3)
                    (return-from isAdherent T)
                    (if (/= 0 (aref port (+ c 1) (+ i min_col)))
                        (return-from isAdherent T)
                    )
                )
        )
        NIL
    )
)

(defun isSameCombination (l1 l2)
    (= 4 (list-length (intersection l1 l2 :test 'equal)))
)

(defun isCombinationInList (comb my_list)
    (loop for x in my_list
        do 
            (if (isSameCombination comb x) 
                (return-from isCombinationInList T)
            )
    )
    NIL
)

(defun concatenate_paths (l1 l2)
    (let 
        ((l_u l1))
        (loop for x in l2
            do 
                (if x
                    (let ()
                        (if (not l_u) 
                            (setq l_u (list x))
                            (if (not (isCombinationInList x l_u))
                                (setq l_u (nconc l_u (list x)))
                            )
                            
                        )
                        
                    )
                )   
        )
        (return-from concatenate_paths l_u)
    )
    
)

(defun nshuffle (sequence)
  (loop for i from (length sequence) downto 2
        do (rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun areFourNeighbours (a b)
    (let 
        ((d_row (- (nth 0 a) (nth 0 b)))
        (d_col (- (nth 1 a) (nth 1 b))))

        (or 
            (and (= 0 d_col) (= 1 (abs d_row)))
            (and (= 0 d_row) (= 1 (abs d_col)))
        )
    )
)

(defun existsIsolatedPiece (path)
    (let ()
        (dotimes (i (list-length path))
            (let ((b NIL))
                (dotimes (j (list-length path))
                    (if (/= i j)
                        (setq b (or b (areFourNeighbours (nth i path) (nth j path))  ))
                    )
                    (if b (return))
                )
                (if (not b) (return-from existsIsolatedPiece T))
            )
        )
        NIL
    )
)

(defun dfs(port row col path)
    (destructuring-bind (rows cols) (array-dimensions port)
        (let
            (
                (tutti NIL)
                (c (list row col))
                (exploration_list (nshuffle (loop :for n :below 8 :collect n)))
            )
            (if (or
                    (< row 0)
                    (>= row rows)
                    (< col 0)
                    (>= col cols)
                    (= 1 (aref port row col))
                    (member c path :test 'equal)) (return-from dfs)
            )
            
            (setq path (pair_concatenation path c))

            (if (= 4 (list-length path) )
                (if (not (existsIsolatedPiece path))
                    (return-from dfs (list path))
                    (return-from dfs NIL)
                )
            )

            (loop for x in exploration_list do 
                (let
                    (
                        (delta_x (nth x di8))
                        (delta_y (nth x dj8))
                    )
                    (setq tutti 
                        (concatenate_paths tutti (dfs port (+ row delta_x) (+ col delta_y) path))
                    )
                )
            )
            tutti

        )
    )
)


(defun findAllSolutionForPiece (port zoid debug)
    (let ((n 0)
          (cc NIL)) 
        (if debug (print-matrix port))
        (destructuring-bind (nrows ncols) (array-dimensions port)
                (if debug (print-matrix port))
                (let 
                    ( (exploration_list 
                            (let ((all-elements '()))
                                (dotimes (row nrows)
                                    (dotimes (col ncols)
                                        (if (= 0 (aref port row col))
                                            (push (list row col) all-elements)
                                        )
                                    )
                                )
                                (nshuffle all-elements)
                            )
                        )
                    )
                    (loop for x in exploration_list do 
                        (let
                            (
                                (row (nth 0 x))
                                (col (nth 1 x))
                            )
                            (print x)
                            (setq cc (concatenate_paths cc (dfs port row col NIL)))
                        )
                    )
                    (setq cc (remove-if-not (lambda (x) (isCompatible x zoid NIL)) cc))
                    (setq cc (remove-if-not (lambda (x) (isAdherent port x)) cc))
                    (if debug (loop for x in cc do 
                            (print-matrix-withPiece port x)
                            (isCompatible x zoid T)
                        )
                    )
                    (setq n (+ n (list-length cc)))
                )  
        )
        cc
    )
)
