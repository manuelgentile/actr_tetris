(clear-all)
(ql:quickload "str")
(ql:quickload "cl-csv")
(ql:quickload "vecto")
(ql:quickload "lhstats")
(ql:quickload "opticl")
(ql:quickload "alexandria")


; parameter to collect analytics of the current task
(defparameter *model-data* nil)
; parameter to collect analytics of all the tasks in the experiment
(defparameter *all-model-data* nil)

; Board configuration
(defparameter *board* nil)
; Board configuration after human performed task
(defparameter *board_human* nil)
; Zoid target in the current task
(defparameter *zoid* nil)
; Task id
(defparameter *id* nil)
; Tetris game level in the current task
(defparameter *level* nil)
; Number of maximum attention areas to analize
(defparameter *area_to_analize* 2)
; Number of maximum solutions in the attention area to analize
(defparameter *solutions_per_area* 2)
; Width of the attention area
(defparameter *attention_width* 6)
; Height of the attention area
(defparameter *attention_heigth* 4)

; Number of rotation steps to consider in the mental rotation process
(defparameter *rotation-steps* 4)
; Rotation angle (function of the rotation steps)
(defparameter *rotation-increment* (/ 360 *rotation-steps*))



(defparameter *target-image-dimensions* '((100 100 50 50) (151 100 50 50)
                                          (100 151 50 50) (151 151 50 50)
                                          (100 100 100 100)))

(defparameter *rotated-image-dimensions* '((400 400 50 50) (451 400 50 50)
                                           (400 451 50 50) (451 451 50 50)
                                           (400 400 100 100)))


; parameters of holistic transfor image action (from Peebles model)
(defvar *imaginal-delay-time* 0.1)
(defparameter *perceptual-noise* 2)
(defparameter *angle-noise* 15)


; accepted images difference
(defparameter *accepted-difference* 0.15)



(defparameter *start-time* 0)

(actr-load "ACT-R:models;mental-jigsaw;imagery-functions.lisp")
(actr-load "ACT-R:models;mental-jigsaw;mental-jigsaw-utils.lisp")
(actr-load "ACT-R:models;mental-jigsaw;mental-jigsaw-find-solutions.lisp")
(actr-load "ACT-R:models;mental-jigsaw;mental-jigsaw-heuristics.lisp")


(defun run-model (board board_human zoid level)
    (let* ()
        (setq *board* board)
        (setq *board_human* board_human)
        (setq *zoid* zoid)
        (setq *level* level)
        (delete-all-visicon-features)
        (setf *start-time* (mp-time-ms))
        (run 20)
    )
)

(defun runsim (start_index number_of_tasks solutions_per_area attention_width)
  (let (
        (*model-data* nil)
        (*all-model-data* nil)
        (*solutions_per_area* solutions_per_area)
        (*attention_width* attention_width)
    )
    (add-act-r-command "record-time" 'prova "Save the current mp-time-ms.")
    (monitor-act-r-command "output-key" "record-time")

    (let ((row_index 1))
        (block momentaneo
        (cl-csv:do-csv (row #P"./models/mental/data_app.csv") :separator #\,
            (if (and (> row_index start_index))
                (let* (
                      
                      (id (nth 3 row))
                      (zoid (coerce (nth 15 row) 'character))
                      (level (parse-integer (nth 8 row)))
                      (initialConfiguration (convert_string_array (nth 7 row)))
                      (finalConfiguration (convert_string_array (nth 6 row)))
                    )
                          (let* (
                              )
                              (setq *id* id)
                              (print zoid)
                              (reset)
                              (delete-all-visicon-features)
                              (install-device '("motor" "keyboard"))
                              (run-model initialConfiguration finalConfiguration zoid level)
                              (setf *model-data* (reverse *model-data*))
                              (push *model-data* *all-model-data*)
                              (setf *model-data* nil)
                          )
                        ;)
                    
                
                )
            )
            (setq row_index (+ row_index 1))
            (print row_index)
            
            (if (> row_index (+ start_index number_of_tasks))  (return-from momentaneo))
        )
        )
    )
    (setf *all-model-data* (reverse *all-model-data*))
    (remove-act-r-command-monitor "output-key" "record-time")
    (remove-act-r-command "record-time")
    (format t "Model RTs: ~A~%" *all-model-data*)
    (with-open-file (str "models/mental/out.csv"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)

        (let ()
            (format str "id;succes;op;time;n_area;i_area;n_sol;i_sol;score~%")
            (loop for task in *all-model-data* do
                (loop for step in task do
                    (let ()
                        (loop for dato in step do
                            (if dato
                                (format str "~A;" dato )
                                (format str ";" )
                            )
                        )
                        (format str "~%")
                    )
                )
                
            )
        )
        
    )
  )
)

(define-model mental-jigsaw

    (sgp :v t :act nil :esc t :lf .63 :mas 1.6 :ga 1.0 :imaginal-activation 1.0 :trace-detail high)
    (sgp :style-warnings nil)

    (chunk-type board matrix matrix_human height width (class c_board))
    (chunk-type tetris_goal (state init) areas solutions index_area index_solution_area
         xloc (pattern-count 1)  (previous-color t) n-solutions id tested_solutions   
         solutions_founded attention_area score board board_human
    )


    (chunk-type (polygon-feature (:include visual-location)) points centre-x centre-y regular)
    (chunk-type (polygon (:include visual-object) (:include polygon-feature)) sides (polygon t) regular points centre-x centre-y)
    (chunk-type (square (:include polygon)) (sides 4) (square t))
    (chunk-type (compound-square (:include square)) sub1 sub2 sub3 sub4)
    (chunk-type imagery-chunk current-image current-color)

    (define-chunks init get-board create-zoid find-solutions mental-imagery-solutions verifico-figure 
        detect_attention_areas get_area analize_next_area visual-solutions evaluate_solution other_solution_or_change_area c_board
        )
    (define-chunks (g1 isa tetris_goal))


    (define-chunks 
        (true isa chunk) (false isa chunk) (square isa chunk) (rotate isa chunk)
        (polygon isa chunk) (compound isa chunk) (start isa chunk) (stop isa chunk) (compare isa chunk)
        (encode isa chunk) (process-pattern isa chunk)
        (encode-zoid-pattern isa chunk) (process-zoid-pattern isa chunk) (pattern-count isa chunk)
        (encode-solution-pattern isa chunk) (attend-solution-pattern isa chunk) (patterns-the-same? isa chunk)
    )

    (goal-focus g1)

    (p init-rule
        =goal>
            ISA     tetris_goal
            state   init
        ==>
            !bind! =id *id*
            !bind! =board *board*
            !bind! =board_human *board_human*
            !bind! =dimension (array-dimensions =board)
            !bind! =height (nth 0 =dimension)
            !bind! =width (nth 1 =dimension)
            !bind! =max_row (get_maximum_row =board)
            !eval! (setq *area_to_analize* (board_to_number_of_area_tbe_v2 =board))
            !eval! (print =board)
            !eval! (print (format NIL "MAX_ROW [~a]" =max_row))
            !eval! (push (list =id nil "init-rule" (- (mp-time-ms) *start-time*)) *model-data*)
            +imaginal>
                ISA           board
                matrix        =board
                matrix_human  =board_human
                width         =width
                height        =height
            =goal>
                state detect_attention_areas 
                id =id
                board         =board
                board_human   =board_human
    )

    (p detect_attention_areas 
        =goal>
            ISA     tetris_goal
            state   detect_attention_areas
            id =id
        =imaginal>
            ISA           board
            matrix        =board
            matrix_human  =board_human
            width         =width
            height        =height

    ==>

        !bind! =areas (sort (isolate_attention_areas =board *attention_width* *attention_heigth*) #'(lambda ( a b ) 
          (< (evaluate_area =board a) (evaluate_area =board b)))
        )
        !bind! =n_area (length =areas)
        !eval! (push (list =id nil "detect_attention_areas" (- (mp-time-ms) *start-time*) =n_area) *model-data*)
        =goal>
            state         create-zoid
            areas         =areas
            index_area    0
        =imaginal>
            ISA           board
            matrix        =board
            matrix_human  =board_human
            width         =width
    )

    
    (p create-target-zoid 
        =goal>
            state create-zoid
            id =id
        ==>
        !eval! (mapcar #'add-visicon-features (create_image_from_path (create_path_zoid *zoid*) *rotated-image-dimensions*))
        =goal>
            state start
        !eval! (push (list =id nil "create_target_zoid" (- (mp-time-ms) *start-time*)) *model-data*)
    )

    (p start-trial
        =goal>
            state start
        ==>
        +visual-location>
            isa visual-location
            > screen-x 300
            color compound
            :attended nil
        =goal>
            state encode-zoid-pattern
    )


    (p attend-to-zoid-pattern
        =goal>
            state encode-zoid-pattern
        =visual-location>
            color =col
            screen-x =screenx
            ?visual>
            state free
        ==>
        +visual>
            isa move-attention
            screen-pos =visual-location
        =goal>
            state process-zoid-pattern
            xloc =screenx
    )


    (p store-zoid-pattern-and-look-for-solution
        =goal>
            state process-zoid-pattern
            xloc =screenx
        =visual>
            points =points
            color =col
            ?imaginal>
            state free
            !eval! (> =screenx 300)
        ==>
        +imaginal>
            isa imagery-chunk
            current-image =points
            current-color =col
        =goal>
            state get_area
    )



    (p get_attention_area 
        =goal>
            ISA           tetris_goal
            state         get_area
            areas         =areas
            index_area    =index_area
            board         =board
            id =id           
    ==>
        !bind! =port (get_area =board =areas =index_area)
        =goal>
            state find-solutions
            attention_area =port
        !eval! (push (list =id nil "get_attention_area" (- (mp-time-ms) *start-time*) nil =index_area) *model-data*)
    )

    (p find-solutions
        =goal>
            ISA             tetris_goal
            state           find-solutions
            id              =id
            index_area      =index_area 
            attention_area  =m
        ==>
        !eval! (print =m)
        !bind! =sol (sort (findAllSolutionForPiece =m *zoid* NIL) 
                    #'(lambda ( a b ) 
                      (evaluate_solutions =m a b)                      
                ))
        !bind! =num-sol (if (listp =sol) (length =sol) 0)
        !eval! (print (format NIL "Solutions founded [~a]" =num-sol))
        !bind! =next_state 
            (if (> =num-sol 0)
                'mental-imagery-solutions 'analize_next_area)
        =goal>
            ISA     tetris_goal
            state   =next_state
            solutions =sol
            n-solutions =num-sol
            index_solution_area 0
        ;!eval! (mental-imagery-solutions-2 =m =sol T)
        !eval! (push (list =id nil "find_solutions" (- (mp-time-ms) *start-time*) nil =index_area =num-sol) *model-data*)
        
    )

    

    (p change_attention_area
        =goal>
            ISA             tetris_goal
            state           analize_next_area
            index_area      =index_area
        !eval! (< =index_area *area_to_analize*)
        ==>
        !bind! =new_index_area (+ =index_area 1)
        =goal>
            ISA                 tetris_goal
            state               get_area
            index_area          =new_index_area
            index_solution_area 0
    )

    (p stop
        =goal>
            ISA             tetris_goal
            id =id
            state           analize_next_area
            index_area      =index_area
        !eval! (>= =index_area *area_to_analize*)
        ==>
        =goal>
            state stop
        ;   !eval! (print (format nil "soluzioni trovate ~a" (length =solutions_founded)))

        !eval! (push (list =id 0 "stop" (- (mp-time-ms) *start-time*)) *model-data*)        
        +manual>
            cmd press-key
            key "m"
    )

    


    (p mental-imagery-solution
        =goal>
            ISA     tetris_goal
            state   mental-imagery-solutions
            solutions =sol
            index_solution_area =index
            index_area =index_area
            id =id
        ==>
        
        !eval! (mapcar #'add-visicon-features (create_image_from_path (nth =index =sol) *target-image-dimensions*))
        =goal>
            state visual-solutions
        !eval! (push (list =id nil "mental-imagery-solution" (- (mp-time-ms) *start-time*) nil =index_area nil =index) *model-data*)


    )


    (p visual-solutions
        =goal>
            ISA     tetris_goal
            state   visual-solutions
            index_solution_area =index
            index_area =area
        ==>
       +visual-location>
             isa visual-location
             < screen-x 300
             color compound
        =goal>
            state encode-solution-pattern
        !eval! (format t "Attention area [~a] Solution [~a] ~%" =area =index)
    )


    (P attend-to-solution-pattern
        =goal>
            state encode-solution-pattern
        =visual-location>
            ?visual>
            state free
        =imaginal>
            current-color =col
        ==>
        +visual>
            cmd move-attention
            screen-pos =visual-location
        =goal>
            state attend-solution-pattern
        =imaginal>
        =visual-location>
    )


    (p different-solution-pattern
        =goal>
            state attend-solution-pattern
        =imaginal>
            current-color =col
        =visual>
            color =actual-col
            - color =col
        ==>
        +visual-location>
            < screen-x 300
            :attended nil
            :nearest current
            - color compound
        =goal>
            state encode-solution-pattern
        =imaginal>
    )


    (p same-solution-pattern
        =goal>
         state attend-solution-pattern
        =visual-location>
         screen-x =screenx
        =imaginal>
         current-color =col
        =visual>
         color =col
         points =target
       ==>
        =goal>
         state process-pattern
         xloc =screenx
         =imaginal>
         =visual>
    )


    (p figures-match
        =goal>
            state process-pattern
            xloc            =screenx
            pattern-count   =pcount
            id              =id
            index_solution_area =index_solution_area
            solutions       =sol
            index_area      =index_area
            areas           =areas
            board           =board
            board_human    =board_human
        =visual>
            points =target
        =imaginal>
            current-image =current
        !eval! (< =screenx 300)
        !bind! =test_diff (calculate_images_difference =current =target )
        !eval! (<=  =test_diff *accepted-difference*)
    ==>
        !eval! (format t "Difference: ~A~%" =test_diff)
        !eval! (format t "Solution n°: ~A~%" =index_solution_area)
        !bind! =area (nth =index_area =areas)
        !bind! =solution (nth =index_solution_area =sol)
        !bind! =aligned_solution (align_local_solution_to_board =area =solution)
        !bind! =human_solution (highlight_zoid_position_human_task =board =board_human nil)
        !eval! (print =area)
        !eval! (print =solution)
        !eval! (print =aligned_solution)
        !bind! =score (evaluate_solution_abs =board =aligned_solution =human_solution)
        !eval! (print (format nil "Score =~a" =score))
        !eval! (push (list =id nil "figures-match" (- (mp-time-ms) *start-time*) nil =index_area nil =index_solution_area) *model-data*)
        =imaginal>
        =goal>
            ISA     tetris_goal
            state   evaluate_solution
            score   =score
    )


    (p figures-do-not-match-rotate
        =goal>
            state process-pattern
            xloc =screenx
            pattern-count =pcount
            index_solution_area =index
            id =id
            index_area =index_area
        =visual>
            points =target
        =imaginal>
            current-image =current
        !eval! (< =screenx 300)
        !eval! (< =pcount *rotation-steps*)
        !bind! =new-pcount (+ =pcount 1)
        !bind! =test_diff (calculate_images_difference =current =target )
        !eval! (> =test_diff *accepted-difference*)
        ?imaginal-action>
            state free
    ==>
        =visual>
        =imaginal>
        +imaginal-action>
            action transform-image
            slots (current-image)
        !eval! (print "The solution does not match, I rotate and try again")       
        =goal>
            pattern-count =new-pcount
        !eval! (format t "Pattern Count: ~A~%" =new-pcount)
        !eval! (format t "Difference: ~A~%" =test_diff)
        !eval! (push (list =id nil "figures-do-not-match-rotate" (- (mp-time-ms) *start-time*) nil =index_area nil =index) *model-data*)        
     )

    (p figures-do-not-match-end_rotation
        =goal>
            state process-pattern
            xloc =screenx
            pattern-count =pcount
            index_solution_area =pindex
            id =id
            index_area =index_area
        =visual>
            points =target
        =imaginal>
            current-image =current
        !eval! (< =screenx 300)
        !eval! (= =pcount *rotation-steps*)
        !bind! =new-pindex (+ =pindex 1)
        !bind! =test_diff (calculate_images_difference =current =target )
        !eval! (> =test_diff *accepted-difference*)
    ==>
        !eval! (print "The solution does not match after trying all the rotations, I'm going to check the next solution")
        !eval! (format t "Difference: ~A~%" =test_diff)
        !eval! (push (list =id nil "figures-do-not-match-end_rotation" (- (mp-time-ms) *start-time*) nil =index_area) *model-data*)
        =imaginal>
        =goal>
            ISA     tetris_goal
            state   other_solution_or_change_area
    )

    (p good_absolute_solution
        =goal>
            ISA     tetris_goal
            state   evaluate_solution
            index_solution_area =index_solution_area
            score   =score
            id =id
            index_area =index_area
        ==>
        =goal>
            state stop       
        !eval! (push (list =id 1 "stop" (- (mp-time-ms) *start-time*) nil =index_area nil =index_solution_area =score) *model-data*)        
        +manual>
            cmd press-key
            key "m"
    )

    (p check-next-solution-in-area
        =imaginal>
        =goal>
            ISA            tetris_goal
            state          other_solution_or_change_area
            index_solution_area =index_solution_area_old
        !bind! =index_solution_area_new (+ =index_solution_area_old 1)
        !eval! (< =index_solution_area_new *solutions_per_area*)
        ==>
        =imaginal>
        =goal>
            ISA     tetris_goal
            state   mental-imagery-solutions
            index_solution_area =index_solution_area_new
            pattern-count 1
    )

    (p check-next-area
        =imaginal>
        =goal>
            ISA            tetris_goal
            state          other_solution_or_change_area
            index_solution_area =index_solution_area_old
        !bind! =index_solution_area_new (+ =index_solution_area_old 1)
        !eval! (>= =index_solution_area_new *solutions_per_area*)
        ==>
        =imaginal>
        =goal>
            ISA     tetris_goal
            state   analize_next_area
            index_solution_area =index_solution_area_new
    )

)