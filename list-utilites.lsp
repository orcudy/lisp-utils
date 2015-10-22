;; [input]
;;   list - list: (x ... nil ... x)
;; [output]
;;   list where top level elements that are nil have been removed: (x ... x)
(defun remove-nil (list)
  (cond
    ; Base case: list is nil
    ((null list) nil)    
    ; Recursive case
    (t (if (null (first list))
	(remove-nil (rest list))
	(cons (first list) (remove-nil (rest list)))))
  )
)

;; [input]
;;   list - top level list
;;   value - value to search for
;; [output]
;;   t if value in list, else nil
(defun contains(list value)
  (cond
    ; Base case: list or value is empty
    ((or (null list) (null value)) nil)
    ; Base case: first element of list is value
    ((equal (first list) value) t)
    ; Recursive case
    (t (contains (rest list) value))
  )
)

;; [input]
;;   list - top level list
;;   value - object in list
;; [output]
;;   index of object in list
(defun index-of (list value) 
  (cond
    ; Base case: list or value is empty
    ((or (null list) (null value)) -1)
    ; Base case: first element of list is value
    ((equal (first list) value) 0)
    ; Recursive case
    (t (+ 1 (index-of (rest list) value)))
  )
)

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
;;   list - top level list
;;   index - 0 indexed location in list
;; [output]
;;   value of object at index index in list (0 indexed)
(defun extract-list-value (list index)
  (cond
    ; Base case: list or index is empty
    ((or (null list) (null index)) nil)
    ; Base case: index = 0
    ((= index 0) (first list))
    ; Recursive case
    (t (extract-list-value (rest list) (- index 1)))
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
;;   list - top level list
;;   index - 0 indexed location in list
;;   symbol - symbol to be placed in list at index index 
;; [output]
;;   top level list where object at index has been replaced with symbol
(defun place-in-list (list index symbol)
  (cond
    ; Base case: list empty
    ((null list) nil)
    ; Base case: negative index
    ((>= 0 index) (cons symbol (rest list)))
    ; Recursive case
    (t (cons (first list) (place-in-list (rest list) (- index 1) symbol)))
  )
)

