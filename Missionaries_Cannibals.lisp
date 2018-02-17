
;; ########################################################################
;;                     ROHIT SINDHU
;;                   SINDH010@UMN.EDU
;; ########################################################################

;; ################## Missionaries and Cannibals ##########################

;; STATE :: (cons (cons ml cl) (cons mr cr) )
;; ml    :: Number of missionaries on left
;; cl    :: Number of cannibals of left
;; mr    :: Number of missionaries on right
;; cr    :: Number of cannibals of right
;; ########################################################################

;; Function to check if state exist in list
;; INPUT
;;      state  :: state to check
;;      list   :: list in which state needs to be checked
;; OUTPUT
;;      return :: t if found or nil if not found
(defun checkList (state list)
    (if (null (find state list :test #'equalp))
        (return-from checkList nil)
        (return-from checkList t)
    )
)
;; ########################################################################

(defvar leftQueue  nil) ; queue for states which we have generated with left moves
(defvar rightQueue nil) ; queue for states which we have generated with right moves
(defvar actions    nil) ; queue for actions are safe to take till now
;; Action will finally conain the solution as list of cons made from actions choosen
;; from rightMoves or leftMoves

;; ########################################################################

;; Valid actions from left to right
;; (i j) designates missionaries and cannibals numbers in boat
(setq rightMoves 
    (list 
        (cons 6 0) (cons 5 0) (cons 4 0) (cons 3 0) (cons 2 0) (cons 1 0)
        (cons 5 1)
        (cons 4 2) (cons 4 1)
        (cons 3 3) (cons 3 2) (cons 3 1)
        (cons 2 2) (cons 2 1)
        (cons 1 1)
    )
)

;; Valid actions from right to left
;; (-i -j) designates missionaries and cannibals numbers in boat
(setq leftMoves 
    (list 
        (cons -1 0) (cons -2 0) (cons -3 0) (cons -4 0) (cons 5 0) (cons -6 0)
        (cons -1 -1)
        (cons -2 -1) (cons -2 -2)
        (cons -3 -1) (cons -3 -2) (cons -3 -3)
        (cons -4 -1) (cons -4 -2)
        (cons -5 -1)
    )
)
;; ########################################################################

;; ####################### Validity Functions ##############################

;; Function to check if all missionaries and cannibals are on left
;; This is be intial state
;; INPUT
;;      state  :: state to check
;; OUTPUT
;;      return :: t if all on left or nil otherwise
(defun checkInitState (state)
    (if (eq nil state) (return-from checkInitState nil))
    (return-from checkInitState
        (and (= 0 (car (cdr state))) (= 0 (cdr (cdr state))))
    )
)

;; Function to check if all missionaries and cannibals are on right
;; This is be goal state
;; INPUT
;;      state  :: state to check
;; OUTPUT
;;      return :: t if all on right or nil otherwise
(defun checkGoalState (state)
    (if (eq nil state) (return-from checkGoalState nil))
    (return-from checkGoalState
        (and (= 0 (car (car state))) (= 0 (cdr (car state))))
    )
)

;; Function to check if the state is valid/ safe
;; INPUT
;;      state  :: state to check
;; OUTPUT
;;      return :: t if state is valid or nil otherwise
(defun validateState (state)
    (if (eq nil state) (return-from validateState nil))

    ;; cannibals can never be more than missionaries
    (if (> (cdr (car state)) (car (car state))) 
        (return-from validateState nil))
    (if (> (cdr (cdr state)) (car (cdr state))) 
        (return-from validateState nil))
    
    ;; any value can not be negative
    (if (> 0 (car (car state))) (return-from validateState nil))
    (if (> 0 (cdr (car state))) (return-from validateState nil))
    (if (> 0 (car (cdr state))) (return-from validateState nil))
    (if (> 0 (cdr (cdr state))) (return-from validateState nil))
    
    ;; All checks done, return it as correct state
    (return-from validateState t)
)
;; ########################################################################

;; ############################ Solvers ###################################

;; Function to generate new system state from given state and action
;; INPUT
;;      state  :: state from which new state is to be generated
;;      act    :: which action to take [from rightMoves and leftMoves]
;; OUTPUT
;;      return :: valid new state or nil otherwise
(defun genrateNewState ( state act ) 
    (if (eq nil state) (return-from genrateNewState nil))
    (if (eq nil act)   (return-from genrateNewState nil))

    (let ( ( tempState nil ) )
        ;; act need to be sent correctly based on the direction
        ;; state     ::   x y -- q w
        ;; new state :: (x - a) (x - b) -- (q + a) (w + b)
        ;; Since values for leftMoves are negative, this will provide correct results
        (setq tempState 
            (cons 
                (cons (- (car (car state)) (car act)) (- (cdr (car state)) (cdr act))) 
                (cons (+ (car (cdr state)) (car act)) (+ (cdr (cdr state)) (cdr act))) 
            )
        )
        ; check if this is valid state
        (if (eq nil (validateState tempState)) (return-from genrateNewState nil) )
        ; if every thing is fine return the state
        (return-from genrateNewState tempState)
    )
)

;; Recursive function to solve the puzzle
;; Searches states and finds the solution
;; INPUT
;;      state  :: current state of the system
;;      dir    :: [ 0 = left || 1 = right ]
;; OUTPUT
;;      return :: t if puzzle is solved or nil otherwise
(defun findSolMissCann (state dir)
    ;; Depending on direction genrate new state 
    (case dir
            (0 ;; LEFT
                (dolist ( act leftMoves )
                    (let ( ( newState (genrateNewState state act) ) )
                        (cond
                            ;; if this state needs to considered further
                            ((eq nil newState) )
                            ((eq t (checkInitState newState)) )
                            ((eq nil (validateState newState)) )
                            ;; if already in leftQueue , then don't consider it further
                            ;; or will go in infinite loop
                            ((eq t (checkList newState leftQueue)) )
                            
                            ;; consider as this is a new state
                            (t 
                                (let (( value nil ))
                                    ;; push act into actions
                                    (setq actions (append (list act) actions))
                                    
                                    ;; check if this is a goal state
                                    (if (eq t (checkGoalState newState)) (return-from findSolMissCann t))
                                    
                                    ;; add new explored state in leftQueue
                                    (setq leftQueue (append leftQueue (list newState)))
                                    
                                    ;; recursively call findSolMissCann with opposite direction
                                    (setq value (findSolMissCann newState 1))
                                    
                                    ;; if this action did not resulted in goal
                                    ;; remove it from actions otherwise return t
                                    (if (eq nil value) (setq actions (cdr actions)) (return-from findSolMissCann t))
                                )
                            )
                        )
                    )
                )
                (return-from findSolMissCann nil)
            )
            (1 ;; RIGHT
                (dolist ( act rightMoves )
                    (let ( ( newState (genrateNewState state act) ) )

                        (cond
                            ;; if this state needs to considered further
                            ((eq nil newState) )
                            ((eq t (checkInitState newState)) )
                            ((eq nil (validateState newState)) )
                            ;; if already in rightQueue , then don't consider it further
                            ;; or will go in infinite loop
                            ((eq t (checkList newState rightQueue)) )
                            
                            ;; consider as this is a new state
                            (t 
                                (let (( value nil ))
                                    ;; push act into actions
                                    (setq actions (append (list act) actions))
                                    
                                    ;; check if this is a goal state
                                    (if (eq t (checkGoalState newState)) (return-from findSolMissCann t))
                                    
                                    ;; add new explored state inrightQueue
                                    (setq rightQueue (append rightQueue (list newState)))
                                    
                                    ;; recursively call findSolMissCann with opposite direction
                                    (setq value (findSolMissCann newState 0))
                                    
                                    ;; if this action did not resulted in goal
                                    ;; remove it from actions otherwise return t
                                    (if (eq nil value) (setq actions (cdr actions)) (return-from findSolMissCann t))
                                )
                            )
                        )
                    )
                )
                (return-from findSolMissCann nil)
            )
    )
)

; Funtion to reset all volatile data structures
(defun reset()
    (setq leftQueue  nil)
    (setq rightQueue nil)
    (setq actions    nil)
)

;; Entry function to create init state and call solution function
;; INPUT
;;      miss   :: number of missionaries
;;      cann   :: number of canibles
;; OUTPUT
;;      return :: action queue which will contain the solution
(defun solveMissCann (miss cann)
    (reset)
    (let ( (init_state (cons (cons miss cann) (cons 0 0))) (status nil))
        ;; dir [ 0 = left || 1 = right ]
        (setq status (findSolMissCann init_state 1))
        (if (not status)(print "Can't Solve"))
    )
    (setq actions (reverse actions))
    (format t "~%~%Solution to missionaries and canibles problem  ::~%" )
        (dolist (elem actions)
            (format t "~S~%" elem)
        )
        (format t "~%~%")
    (return-from solveMissCann t)
)

;; ########################################################################
;; ########################################################################
;; ########################################################################
;; ########################################################################