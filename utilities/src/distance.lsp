;; [input]
;;   source - a source coordinate: (x y)
;;   destinations - list of possible destination coordinates: ( (x y) ... (x y) )
;; [output]
;;   list containt destination coordinate which has closest manhattan distance to source, and manhattan distance to said coordinate: ( (x y) distance )
(defun closest-by-manhattan (source destinations)
  (cond 
    ; Base case: destinations or source empty
    ((or (null destinations) (null source)) nil)
    ; Base case: destination contains one coord
    ((= (length destinations) 1) (list (first destinations) (manhattan-distance (first destinations) source)))
    ; Recursive case
    (t (let ((manhattanDistanceFirst (closest-by-manhattan source (list (first destinations))))
	     (manhattanDistanceRest (closest-by-manhattan source (rest destinations))))
	 (if (> (second manhattanDistanceFirst) (second manhattanDistanceRest)) manhattanDistanceRest manhattanDistanceFirst))
    )
  )
)

;; [input]
;;   source - a source coordinate: (x y)
;;   destination - a destination coordinate: (x y)
;; [output]
;;   manhattan distance between source and destination
(defun manhattan-distance (source destination)
  (+ (abs (- (first source) (first destination)))
     (abs (- (second source) (second destination))))
)

