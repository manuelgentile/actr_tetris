(defun evaluate_area (board area)
    (let  
        (
            (row_start (nth 0 area))
            (col_start (nth 1 area)) 
            (row_end   (nth 2 area))
            (col_end   (nth 3 area))
        )
        (reduce '+ (alexandria:flatten (2d-array-to-list (get-portion board row_start col_start row_end col_end))))
    )
)

(defun get_connected_component (array)
    (let 
        (
            (g (copy-array array))
            (w (make-array (array-dimensions array)))
            (s 0)

        )
        (loop for i from 0 below (array-dimension g 0) 
            do (loop for j from 0 below (array-dimension g 1)
                    do (if (= (aref g i j) 0) 
                        (setf (aref g i j) 1)
                        (setf (aref g i j) 0)
                    )
                )
        )
        (loop for i from 0 below (array-dimension g 0) 
            do (loop for j from 0 below (array-dimension g 1)
                    do (if (and (= (aref g i j) 1) (= (aref w i j) 0))
                            (let ()
                                (setq s (+ s 1))
                                (setq w (dfs_connected_component g w i j s))
                            )
                    )
                )
        )
        s
    )
)

(defun get_connected_component_with_solution (area sol)
    (let ((port (copy-array area)))
        (loop for x in sol do
            (setf (aref port (nth 0 x) (nth 1 x) ) 1)
        )
        (get_connected_component port)
    )
)

(defun get_one_last_row (area sol)
    (let ((port (copy-array area)))
        (loop for x in sol do
            (setf (aref port (nth 0 x) (nth 1 x) ) 1)
        )
        (reduce '+ (array-slice port 3))
    )
)

(defun evaluate_solutions(m a b)
    (let 
        (
            (conn_comp_a (get_connected_component_with_solution m a))
            (conn_comp_b (get_connected_component_with_solution m b))
        )
        (or 
            (< conn_comp_a conn_comp_b)
            (and 
                (= conn_comp_a conn_comp_b)
                (> (get_one_last_row m a) (get_one_last_row m b))
            )
        )
    )
)

(defun evaluate_solution_abs (board solution solution_human)
    (let* 
        (
           (board_difference (calculate_board_difference board solution solution_human))
           (dims  (array-dimensions board_difference))
           (x 0)
        )
        (destructuring-bind (rows cols) dims
            (dotimes (row rows)
                (dotimes (col cols)
                    (setf x (+ x (abs (aref board_difference row col))))
                )
            )
        )
        (terpri)
        (print "----------------")
        (print-matrix board_difference)
        (print "----------------")
        (terpri)
        x
    )
)

; transform-image action is taken from the holistic model of Peebles
(defun transform-image (slot-to-change)
    (let ((delay (randomize-time *imaginal-delay-time*)))
        (schedule-mod-buffer-chunk 'imaginal
                                   (list slot-to-change
                                         (rotate-counter-clockwise-around-point
                                          (chunk-slot-value-fct (buffer-read 'imaginal) slot-to-change)
                                          (+ *rotation-increment*
                                             (act-r-noise *perceptual-noise*))))
                                   delay)
        (schedule-event-relative delay 'set-imaginal-free)
    )
)