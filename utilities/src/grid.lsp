(load `../src/general.lsp)

;; [input]
;;   grid - list of lists: ( (...) ... (...) )
;;   x - index of element in grid (left column is 0)
;;   y - index of element in grid (top row is 0)
;; [output]
;;   value of object at location (x, y) in grid   
(defun extract-grid-value (grid x y)
  (cond 
    ; Base case: grid = nil, x = nil, y = nil, x < 0, y < 0
    ((or (null grid) (null x) (null y) (> 0 x) (> 0 y)) nil)
    ; Base case: y = 0
    ((= y 0) (extract-list-value (first grid) x))
    ; Recursive case
    (t (extract-grid-value (rest grid) x (- y 1)))
  )
)

;; [input]
;;   grid - list of lists: ( (...) ... (...) )
;;   x - index of element in grid (left column is 0)
;;   y - index of element in grid (top row is 0)
;;   symbol - to be placed in grid at location x, y
;; [output]
;;   grid where object at location (x, y) has been replaced with symbol
(defun place-in-grid (grid x y symbol)  
  (cond     
    ; Base case: grid empty
    ((null grid) nil)
    ; Base case: 1 x 1 grid
    ((atom (first grid)) (list element))
    ; Recursive case
    (t (if (= y 0)
	   (cons (place-in-list (first grid) x symbol) (rest grid))
	   (cons (first grid) (place-in-grid (rest grid) x (- y 1) symbol))
       )
    )
  )
)

;; [input]
;;   grid - list of lists: ( (...) ... (...) )
;;   value - value to search for in grid
;; [output]
;;   number of times value occurs in grid
(defun occurrences-in-grid (grid value)
  (cond
    ; Base case: grid or value is nil
    ((or (null grid) (null value)) 0)
    ; Recursive case
    (t (+ (occurrences-in-list (first grid) value) (occurrences-in-grid (rest grid) value)))
  )
)
