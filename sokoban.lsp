(load `sokoban-globals.lsp)
(load `list-utilities.lsp)

;; [input]
;;   state - list of lists representing sokoban game board
;; [output]
;;   list of valid states (states represent sokoban game board and where determined by making one move from current game board)
(defun next-states (state)
  (let* ((states (list (try-move state 0 1)
		       (try-move state 0 -1)
		       (try-move state 1 0)
		       (try-move state -1 0)))
	 (states (remove-nil states)))
    states
  )
)

;; [input]
;;   grid - list of lists representing sokoban game board
;;   x - horizontal movement (-1 = left, 1 = right)
;;   y - vertical movement (-1 = up, 1 = down)
;; [output]
;;   if move valid, the resulting game board after move, else nil
(defun try-move (grid x y)
  (let* (
	 ; position of keeper, position one space from keeper (in direction of move), and position two spaces from keeper (in direction of move)
	 (keeperPosition (keeper-position grid))
	 (keeperPosition+ (list (+ (first keeperPosition) x) (+ (second keeperPosition) y)))
	 (keeperPosition++ (list (+ (first keeperPosition+) x) (+ (second keeperPosition+) y)))

	 ; symbols at keeperPosition+ and keeperPosition++
	 (preSymbol+ (extract-grid-value grid (first keeperPosition+) (second keeperPosition+)))
	 (preSymbol++ (extract-grid-value grid (first keeperPosition++) (second keeperPosition++)))

	 ; error check: symbols could be null because of grid configuration
	 (preSymbol+ (if (null preSymbol+) wall preSymbol+))
	 (preSymbol++ (if (null preSymbol++) wall preSymbol++))

	 ; symbol to place at keeper's current position
	 (postKeeperSymbol (if (= (extract-grid-value grid (first keeperPosition) (second keeperPosition)) keeperstar)
			   star
			   blank))

	 ; symbol to place at keeper's new position
	 (postSymbol+ (cond
		        ; keeper into blank
		        ((= preSymbol+ blank) keeper)
			; keeper into box
		        ((= preSymbol+ box) keeper)
			; keeper into star
			((= preSymbol+ star) keeperstar)
			; keeper into boxstar
			((= preSymbol+ boxstar) keeperstar)
			(t nil)))

	 ; symbol to place at space adjacent (in direction of move) to keeper's new position
	 (postSymbol++ (cond
			 ; wall
			 ((= preSymbol++ wall)
			  wall)
		         ; blank into blank
			 ((and (= preSymbol+ blank)
			       (= preSymbol++ blank))			       
			  blank)
			 ; box/boxstar into blank
			 ((and (or (= preSymbol+ box)
				   (= preSymbol+ boxstar))
			       (= preSymbol++ blank))
			  box)
			 ; box/boxstar into star
			 ((and (or (= preSymbol+ box)
				   (= preSymbol+ boxstar))
			       (= preSymbol++ star))
			  boxstar)
			 (t nil))))
    (cond 
      ; invalid move, return nil
      ((or (null postSymbol+) (null postSymbol++)) nil)
      ; valid move
      (t (let* ((resultingGrid (place-in-grid grid (first keeperPosition) (second keeperPosition) postKeeperSymbol))
		(resultingGrid (place-in-grid resultingGrid (first keeperPosition+) (second keeperPosition+) postSymbol+))
		(resultingGrid (place-in-grid resultingGrid (first keeperPosition++) (second keeperPosition++) postSymbol++)))
	   resultingGrid))
    )
  )
)

;; [input]
;;   state - list of lists: ( (...) ... (...) ) which represents a sokoban board
;; [output]
;;   t if state is winning configuration in sokoban, else nil
(defun goal-test (state)
  (cond 
    ; Base case: s is empty
    ((null state) t)
    ; Base case: s is atom
    ((atom state) (if (equal state box) nil t))
    ; Recursive case
    (t (and (goal-test (first state)) (goal-test (rest state))))
  )   
)

;; [input]
;;   grid - list of lists: ( (...) ... (...) )
;; [output]
;;   list of x and y coordinates of keeper: (x y)
(defun keeper-position (grid)
  (cond
    ; Base case: grid empty
    ((null grid) nil)
    ; Base case: keeper in first row
    ((contains (first grid) keeper) (list (index-of (first grid) keeper) 0))
    ; Base case: keeper on star in first row
    ((contains (first grid) keeperstar) (list (index-of (first grid) keeperstar) 0))
    ; Recursive case
    (t (let ((position (keeper-position (rest grid))))
	 (list (first position) (+ 1 (second position)))
       )
    )
  )
)
