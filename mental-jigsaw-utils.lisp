(defparameter di '(0 1  0 -1))
(defparameter dj '(1 0 -1  0))
(defparameter di8 '(-1 -1 -1  0 0  1 1 1))
(defparameter dj8 '(-1  0  1 -1 1 -1 0 1))

(defun 2d-array-to-list (array)
  (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))

(defun copy-array (an-array)
  (let* ((dims      (array-dimensions an-array))
         (new-array (make-array dims)))
    (destructuring-bind (rows cols) dims
      (dotimes (row rows new-array)
        (dotimes (col cols)
          (setf (aref new-array row col) (aref an-array row col))))))
)

(defun print-matrix (array)
  (loop for i from 0 below (array-dimension array 0)
    do (loop for j from 0 below (array-dimension array 1)
             do (princ (aref array i j))
                (if (= j (1- (array-dimension array 1)))
                    (terpri)
                    (princ #\Space)))))


(defun print-2d-array-as-table (array)
  (loop for i below (array-total-size array) do
    (if (zerop (mod i (array-dimension array 0)))
        (terpri)
        (princ #\Space))
    (princ (row-major-aref array i))))

(defun print-matrix-withPiece (port path)
    (let ((port2 (copy-array port)))
        (print "----------------")
        (terpri)
        (loop for x in path do
            (setf (aref port2 (nth 0 x) (nth 1 x) ) 2)
        )
        (print-matrix port2)
        (print "----------------")
        (terpri)
        port2
    )
)

(defun create-board (lines columns)
    (let 
        ((m (make-array `(,lines ,columns))))
        (print-2d-array-as-table m)
        m
    )
)

(defun get-portion (an-array row0 col0 row1 col1)
  (destructuring-bind (rows cols) (array-dimensions an-array)
    (let* ((row1 (if (or (null row1) (> 0 row1)) (1- rows) row1))
           (col1 (if (or (null col1) (> 0 col1)) (1- cols) col1))
           (nrows    (1+ (- row1 row0)))
           (ncols    (1+ (- col1 col0))))
      (cond ((< row1 row0)      (error "get-portion: ROW1 may not be larger than ROW0!"))
            ((< col1 col0)      (error "get-portion: COL1 may not be larger than COL0!"))
            ((< row0 0)         (error "get-portion: ROW0 must be positive!"))
            ((< col0 0)         (error "get-portion: COL0 must be positive!"))
            ((< rows 1)         (error "get-portion: AN-ARRAY must not be empty!"))
            ((< cols 1)         (error "get-portion: AN-ARRAY must not be empty!"))
            ((< (1- rows) row0) (error "get-portion: ROW0 too large!"))
            ((< (1- cols) col0) (error "get-portion: COL0 too large!"))
            ((< (1- rows) row1) (error "get-portion: ROW1 too large!"))
            ((< (1- cols) col1) (error "get-portion: COL1 too large!")))
      (let ((new-array (make-array (list nrows ncols))))
        (dotimes (row nrows new-array)
          (dotimes (col ncols)
            (setf (aref new-array row col) (aref an-array (+ row0 row) (+ col0 col)))))))))

(defun max_x_dim (path dim)
    (reduce #'max (mapcar (lambda (x) (nth dim x)) path))
)

(defun min_x_dim (path dim)
    (reduce #'min (mapcar (lambda (x) (nth dim x)) path))
)

(defun get-portion-from-place (an-array min_row max_row min_col max_col)
    (let* (
            (dx (- max_col min_col))
            (dy (- max_row min_row))
            (spanx (- 3 dx))
            (spany (- 3 dy))
            (x nil)
            (y (- max_row 3))
        )
        (if (> min_col spanx)
            (setq x (- min_col spanx))
            (setq x 0)
        )
        (if (> min_row spany)
            (setq y (- min_row spany))
            (setq y 0)
        )
        (print (format NIL "da (~a,~a) a (~a,~a)"  min_col min_row max_col max_row))
        (print (format NIL "~a ~a"  spanx spany))
        (print (format NIL "~a ~a"  x y))
        (get-portion an-array y x (+ y 3) (+ x 3))
    )
)

(defun is_empty (array)
    (let ((empty T))
        (loop for i from 0 below (array-dimension array 0)
            do (loop for j from 0 below (array-dimension array 1)
                 do 
                    (if (/= (aref array i j) 0)
                        (setq empty NIL)
                    )
                )
        )
        empty
    )
)

(defun board_to_number_of_area_tbe_v2 (board)
    (destructuring-bind (rows cols) (array-dimensions board)
        (let 
            (
                (prop (/ (reduce '+ (alexandria:flatten (2d-array-to-list board))) (* rows cols)))
            )
            (if (> prop 0.8) 1
                (if (> prop 0.6) 2
                    (if (> prop 0.4) 3
                        4
                    )
                )
            )
        )
    )    
)

(defun board_to_number_of_area_tbe (row level)
    (if (> level 3)
        (if (> row 15) 2
            (if (> row 10) 1
                    0
            )
        )
        (if (> row 15) 3
            (if (> row 10) 2
                (if (> row 10) 1
                    0
                )
            )
        )
    )
)

(defun get_maximum_row (board)
    (destructuring-bind (rows cols) (array-dimensions board)
        (let*
            ( 
                (row_end NIL)
            )
            (loop for col from 0 below cols do 
                (let 
                    (
                        (row 0)
                    )                
                    (loop while (and (= (aref board row col) 0) (< row (- rows 1))) do (setf row (+ row 1)))
                    (if (not row_end) (setf row_end row)
                        (if (< row row_end) (setf row_end row))
                    )
                )
            )
            row_end
        )
    )
)


(defun isolate_attention_areas (board attention-width attention-heigth)
    (let ((lista)
        )
        (destructuring-bind (rows cols) (array-dimensions board)
            (loop for index from 0 below (+ (- cols attention-width) 1) do 
                (let*
                    ( 
                        (col_start index) 
                        (col_end   (+ col_start (- attention-width 1)))
                        (row_start NIL)
                        (row_end NIL)
                        (alt NIL)
                    )
                    (loop for col from col_start to col_end do 
                        (let ((row 0)
                               
                               )                
                            (loop while (and (= (aref board row col) 0) (< row (- rows 1))) do (setf row (+ row 1)))
                            (if (not row_end) (setf row_end row)
                                (if (> row row_end) (setf row_end row))
                            )
                        )
                    )
                    ;(setf row_end (- row_end 1))
                    (setf row_start (- row_end (- attention-heigth 1)))
                    (setf row_start (if (< row_start 0) 0 row_start))
                    (setf alt (+ (- row_end row_start) 1))
                    (print (format NIL "da (~a,~a) a (~a,~a)"  row_start col_start row_end col_end))
                    (if (> alt 3)
                        (push (list row_start col_start row_end col_end ) lista)
                    )
                )
            )
        )
        lista
    )
)

(defun get_area(board areas n)
    (let*  
        (
            (area (nth n areas))
            (row_start (nth 0 area))
            (col_start (nth 1 area)) 
            (row_end   (nth 2 area))
            (col_end   (nth 3 area))
        )
        (print (format NIL "from (~a,~a) to (~a,~a)"  row_start col_start row_end col_end))
        (get-portion board row_start col_start row_end col_end)
    )
)





(defun print-solutions (solutions)

    (loop for x in solutions do 
        (let ()
                (print x)
                (print "-")
        )
    )
)

(defun array-slice (arr row)
    (make-array (array-dimension arr 1) 
      :displaced-to arr 
       :displaced-index-offset (* row (array-dimension arr 1))))

(defun dfs_connected_component (g w i j s)
    (let ()
        (setf (aref w i j) s)
        (loop for index from 0 below 4 do 
            (let 
                (
                    (ni (+ i (nth index di)))
                    (nj (+ j (nth index dj)))

                )
                (if (and (> ni -1) (> nj -1) (< ni (array-dimension g 0)) (< nj (array-dimension g 1)))                    
                        (if (and (= (aref g ni nj) 1) (= (aref w ni nj) 0))
                                    (dfs_connected_component g w ni nj s)
                        )   
                )
            )
        )
        w
    )
)





(defun align_local_solution_to_board (area sol)
    (let* 
        (
            (delta_row (nth 0 area))
            (delta_col (nth 1 area))
            (new_sol (copy-tree sol))
        )
        (print "sono qua2")
        (loop for i from 0 to 3 do
            (setf (nth 0 (nth i new_sol)) (+ (nth 0 (nth i new_sol)) delta_row))
            (setf (nth 1 (nth i new_sol)) (+ (nth 1 (nth i new_sol)) delta_col))
        )
        new_sol
    )
)

(defun calculate_board_difference (board solution solution_human)
    (let (
            (board_model (copy-array board))
            (board_human (copy-array board))
            (board_diff (copy-array board))
        )
        (loop for x in solution do
            (setf (aref board_model (nth 0 x) (nth 1 x) ) 2)
        )
        (loop for x in solution_human do
            (setf (aref board_human (nth 0 x) (nth 1 x) ) 2)
        )
        (let* 
            (
                (dims  (array-dimensions board_diff))
            )
            (destructuring-bind (rows cols) dims
                (dotimes (row rows)
                    (dotimes (col cols)
                        (setf (aref board_diff row col) (- (aref board_human row col) (aref board_model row col)))
                    )
                )
            )
        )
        board_diff
    )
)



(defun calculate_boards_difference (board_model board_human)
    (let (
            (x 0)
            (dims  (array-dimensions board_model))
        )
        (destructuring-bind (rows cols) dims
            (dotimes (row rows)
                (dotimes (col cols)
                    (setf x (+ x (abs (- (aref board_human row col) (aref board_model row col)))))                 
                )
            )
        )
        x
    )
)


(defun mid-point (pval1 pval2)
  (let* ((sorted-vals (sort (list pval1 pval2) #'<))
         (mid-pt (/ (float (- (second sorted-vals)
                              (first sorted-vals))) 2.0)))
    (round (+ mid-pt (first sorted-vals)))))


(defun my-create-target-image (dims points-list)
  `((isa (polygon-feature square)
         screen-x ,(first (first dims)) screen-y ,(second (first dims))
         kind (polygon nil) value (polygon "sub1")
         points ,(list (first points-list))
         height ,(third (first dims)) width ,(fourth (first dims))
         centre-x ,(mid-point (first (first dims)) (+ (first (first dims))
                                                      (fourth (first dims))))
         centre-y ,(mid-point (second (first dims)) (+ (second (first dims))
                                                       (third (first dims))))
         color black regular true)
    (isa (polygon-feature square)
         screen-x ,(first (second dims)) screen-y ,(second (second dims))
         kind (polygon nil) value (polygon "sub2")
         points ,(list (second points-list))
         height ,(third (second dims)) width ,(fourth (second dims))
         centre-x ,(mid-point (first (second dims)) (+ (first (second dims))
                                                       (fourth (second dims))))
         centre-y ,(mid-point (second (second dims)) (+ (second (second dims))
                                                        (third (second dims))))
         color black regular true)
    (isa (polygon-feature square)
         screen-x ,(first (third dims)) screen-y ,(second (third dims))
         kind (polygon nil) value (polygon "sub3")
         points ,(list (third points-list))
         height ,(third (third dims)) width ,(fourth (third dims))
         centre-x ,(mid-point (first (third dims)) (+ (first (third dims))
                                                      (fourth (third dims))))
         centre-y ,(mid-point (second (third dims)) (+ (second (third dims))
                                                       (third (third dims))))
         color black regular true)
    (isa (polygon-feature square)
         screen-x ,(first (fourth dims)) screen-y ,(second (fourth dims))
         kind (polygon nil) value (polygon "sub4")
         points ,(list (fourth points-list))
         height ,(third (fourth dims)) width ,(fourth (fourth dims))
         centre-x ,(mid-point (first (fourth dims)) (+ (first (fourth dims))
                                                       (fourth (fourth dims))))
         centre-y ,(mid-point (second (fourth dims)) (+ (second (fourth dims))
                                                        (third (fourth dims))))
         color black regular true)
    (isa (polygon-feature compound-square)
         screen-x ,(first (fifth dims)) screen-y ,(second (fifth dims))
         kind (polygon nil) value (polygon "compound")
         ; Pass a list of two lists, one for the location and one for the object
         points ,(list (concatenate 'list
                               (first points-list) (second points-list)
                               (third points-list) (fourth points-list)))
         height ,(third (fifth dims)) width ,(fourth (fifth dims))
         centre-x ,(mid-point (first (fifth dims)) (+ (first (fifth dims))
                                                      (fourth (fifth dims))))
         centre-y ,(mid-point (second (fifth dims)) (+ (second (fifth dims))
                                                       (third (fifth dims))))
         color compound regular true
         sub1 (nil "sub1")
         sub2 (nil "sub2")
         sub3 (nil "sub3")
         sub4 (nil "sub4"))))


(defun create_image_from_path (path dims)
    (let (
          (new_image nil))
        (loop for x in path do
            (let ((r (nth 0 x))
                  (c (nth 1 x))
                )
                (push (list 
                            (list (* 10 r) (* 10 c)) 
                            (list (* 10 (+ 1 r)) (* 10 c))
                            (list (* 10 (+ 1 r)) (* 10 (+ 1 c)))
                            (list (* 10 r) (* 10 (+ 1 c)))                         
                        )
                    new_image )
            )
        )
        (my-create-target-image dims  new_image)
    )
)

(defun create_path_zoid (zoid)
    (let 
        (
          (new_zoid nil)
        )
        (if (char= zoid #\J)
            (setq new_zoid '((0 2) (0 1) (1 1) (2 1)))
        )
        (if (char= zoid #\L)
            (setq new_zoid '((0 1) (1 1) (2 1) (2 2)))
        )
        (if (char= zoid #\S)
            (setq new_zoid '((0 1) (1 1) (1 2) (2 2)))
        )
        (if (char= zoid #\Z)
            (setq new_zoid '((0 2) (1 2) (1 1) (2 1)))
        )

        (if (char= zoid #\T)
            (setq new_zoid '((0 1) (1 1) (2 1) (1 2)))
        )
        (if (char= zoid #\I)
            (setq new_zoid '((0 1) (1 1) (2 1) (3 1)))
        )
        (if (char= zoid #\O)
            (setq new_zoid '((0 1) (0 2) (1 1) (1 2)))
        )
        new_zoid
    )
)

(defun max_consecutive_dim (path dim)
    (let 
        (
            (v (sort (mapcar (lambda (x) (nth dim x)) path) #'<))
            (n_max 0)
            (n 1)
        )
        (loop for i from 0 below (list-length v) do 
            (block ss
                (let ()
                    (setq n 1)
                    (loop for j from (+ 1 i) below (list-length v) do 
                        (if (= (nth j v) (nth i v)) (setq n (+ 1 n)) (return-from ss))
                    )
                )
            )
            (if (> n n_max) (setq n_max n))
        )
        n_max
    )
)

(defun mean_x_dim (path dim)
    (let* 
        (
            (v (mapcar (lambda (x) (nth dim x)) path))
            (v_sum (reduce '+ v))
            (n (list-length v))
        )
        (if (> n 0) (/ v_sum n))
    )
)

(defun span (path dim)
    (+ 1 (- (max_x_dim path dim) (min_x_dim path dim)))
)


(defun pair_concatenation (lista coppia)
        (if (= (list-length lista) 0)
            (setq lista (list coppia))
            (setq lista (cons coppia lista))
        )
    
)

(defun findEmptyPieceLastRow (port)
    (destructuring-bind (rows cols) (array-dimensions port)
        (let ((n -1))
            (dotimes (col cols n)
                (if (= 0 (aref port (1- rows) col))
                        (setq n col)
                    )
                )
            )
        )
    )

(defun numberFullPiecesLastRow (port)
    (destructuring-bind (rows cols) (array-dimensions port)
        (let ((n 0))
            (dotimes (col cols n)
                (if (= 1 (aref port (1- rows) col))
                        (setq n (1+ n))
                    )    

                )
            )
        )
    )


(defun convert_string_array( txt )
    (let (
            (rows (str:split #\newline txt))
            (matrix (make-array '(21 10)))
         )
        (loop for i from 0 below 21
            do (let ((row (nth i rows)))
                    (loop 
                        for c across row 
                        for j from 0 

                        do (if (char= c #\0)
                                (setf (aref matrix i j) 0)
                                (setf (aref matrix i j) 1)
                            )
                    )
                )
        )
        matrix
    )
)

(defun highlight_zoid_position_human_task (initialConfiguration finalConfiguration debug)
    (let* (          
          (path NIL)
        )
        (loop for i from 0 below 21
            do
            (loop for j from 0 below 10
                do (if (/= (aref finalConfiguration i j) (aref initialConfiguration i j))
                        (let ()
                            (setf (aref finalConfiguration i j) 2)
                            (setq path (nconc path (list (list i j))))
                    )
                )
            )
        )
        (if debug (print finalConfiguration))
        path
    )
)





(defun my-sum (dr dc x)
    (list (+ dr (nth 0 x)) (+ dc (nth 1 x)))
)

(defun align (b dr dc)
    (let* ( (r (mapcar (lambda (x) (my-sum dr dc x)) b))
          )
          r
    )
)

(defun crea-png-zoid (file allpoints mx my)
    (vecto:with-canvas (:width mx :height my)
        (dotimes (n 4)
            (let* (
                (points (subseq allpoints (* n 4) (* (+ n 1) 4 )))
                (p (nth 3 points)))
                (vecto:move-to (nth 0 p) (nth 1 p))
                (loop for x in points do 
                    (vecto:line-to (nth 0 x) (nth 1 x))
                )
                (vecto:fill-path)       
                (vecto:save-png file)
            )
        )
    )
)

(defun create_image_file (filename zoid)
        (let* (
                (dx (min_x_dim zoid 0))
                (dy (min_x_dim zoid 1))
                (zoidn (align zoid (* -1 dx) (* -1 dy)))
                (mx (round (max_x_dim zoidn 0)))
                (my (round (max_x_dim zoidn 1)))
            )
            (crea-png-zoid filename zoidn mx my)
        )
)

(defun calculate_images_difference (current target)
    (let* 
        (
        )
        (create_image_file "current.png" current)
        (create_image_file "target.png" target)
        (let* ( (a (opticl::resize-image (opticl::read-png-file "current.png") 50 50))
                (b (opticl::resize-image (opticl::read-png-file "target.png") 50 50))
                (count_b 0)
                (count_t 0)
               )
                (loop for i from 0 to 49 do 
                    (loop for j from 0 to 49 do

                        (if (or (and (= 255 (aref a i j 3)) (= 0 (aref b i j 3)))
                                (and (= 0 (aref a i j 3)) (= 255 (aref b i j 3))))
                            (setq count_b (+ 1 count_b))
                        )

                        (if (or (= 255 (aref a i j 3)) (= 255 (aref b i j 3)))
                            (setq count_t (+ 1 count_t))
                        )

                    )
                )
                (/ (float count_b) (float count_t))
        
        )
    )
)
