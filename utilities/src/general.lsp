;; [input]
;;   number - any number
;; [output]
;;  absolute value of number
(defun abs (number)
  (if (>= number 0) number (- number))
)

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
;;   value - value to remove from list
;; [output]
;;   list where all instances of value have been removed from list
(defun remove (list value)
  (cond
    ; Base case: list or value is empty
    ((or (null list) (null value)) nil)
    ; Base case: list is atom
    ((atom list) (if (equal list value) nil (list list)))
    ; Recursive case
    (t (append (remove (first list) value) (remove (rest list) value)))
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

;; [input]
;;   list - list: ( x ... x )
;;   value - value to search for in list
;; [output]
;;   number of times value occurs in list
(defun occurrences-in-list (list value)
  (cond
    ; Base case: list or value empty
    ((or (null list) (null value)) 0)
    ; Base case: list is atom
    ((atom list) (if (equal list value) 1 0))
    ; Recursive case
    (t (+ (occurrences-in-list (first list) value) (occurrences-in-list (rest list) value)))
  )
)