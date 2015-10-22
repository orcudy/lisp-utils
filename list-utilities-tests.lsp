(load `list-utilities.lsp)

(setq verbose nil)
(defmacro unit-test (expr result msg)
  `(if (equal ,expr ,result)
       (if verbose 
	   (format t "~% Passed: ~S | returned ~S, expected ~S" ,msg ,expr ,result)
	   (format t "~% Passed: ~S" ,msg))
        (if verbose
	    (format t "~%Failed: ~S | returned ~S, expected ~S" ,msg ,expr ,result)
	    (format t "~%Failed: ~S" ,msg))
   )
)

(format t "~%~%-- contains --~%")
(unit-test (contains `() 4) nil "list is nil")
(unit-test (contains `(1 2 3) nil) nil "value is nil")
(unit-test (contains `(1 2 3 4 5) 1) t "contains value (beginning of list)")
(unit-test (contains `(1 2 3 4 5) 4) t "contains value (middle of list)")
(unit-test (contains `(1 2 3 4 5) 5) t "contains value (end of list)")
(unit-test (contains `(1 2 3 4 5) 10) nil "does not contain value")

(format t "~%~%-- index-of --~%")
(unit-test (index-of `() 4) -1 "list is nil")
(unit-test (index-of `(1 2 3) nil) -1 "value is nil")
(unit-test (index-of `(1 2 3 4 5) 1) 0 "at first index")
(unit-test (index-of `(1 2 3 4 5) 4) 3 "at middle index")
(unit-test (index-of `(1 2 3 4 5) 5) 4 "at end index")

(format t "~%~%-- extract-grid-value --~%")
(unit-test (extract-grid-value `() 0 0) nil "grid is nil")
(unit-test (extract-grid-value `(1 2 3) nil 0) nil "x is nil")
(unit-test (extract-grid-value `(1 2 3) 0 nil) nil "y is nil")
(unit-test (extract-grid-value `(1 2 3) -1 0) nil "x is negative")
(unit-test (extract-grid-value `(1 2 3) 0 -1) nil "y is negative")

(setq grid `((00 10 20 30 40)
	     (01 11 21 31 41)
	     (02 12 22 32 42)
	     (03 13 23 33 43)
	     (04 14 24 34 44)))
	     
(unit-test (extract-grid-value grid 0 0) 00 "value at (0, 0)")
(unit-test (extract-grid-value grid 2 3) 23 "value at (2, 3)")
(unit-test (extract-grid-value grid 3 4) 34 "value at (3, 4)")
(unit-test (extract-grid-value grid 4 4) 44 "value at (4, 4)")

(format t "~%~%-- extract-list-value --~%")
(unit-test (extract-list-value `() 0) nil "list is nil")
(unit-test (extract-list-value `(1 2 3) nil) nil "value is nil")
(unit-test (extract-list-value `(1 2 3 4 5) 0) 1 "value at first index")
(unit-test (extract-list-value `(1 2 3 4 5) 2) 3 "value at middle index")
(unit-test (extract-list-value `(1 2 3 4 5) 4) 5 "value at end index")

(format t "~%~%-- place-in-list --~%")
(unit-test (place-in-list `() 0 9) nil "list is nil")
(unit-test (place-in-list `(1 2 3 4 5) 0 9) `(9 2 3 4 5) "place at start index")
(unit-test (place-in-list `(1 2 3 4 5) 2 9) `(1 2 9 4 5) "place at middle index")
(unit-test (place-in-list `(1 2 3 4 5) 4 9) `(1 2 3 4 9) "place at end index")

(format t "~%~%-- place-in-grid --~%")
(unit-test (place-in-grid `() 0 0 10) nil "grid is nil")

(setq grid `((1 2 3 4 5)))
(setq expected `((1 2 @ 4 5)))
(unit-test (place-in-grid grid 2 0 `@) expected "5 x 1 grid, replace at (2, 0)")

(setq grid `((10 11 12 13 14)
	     (20 21 22 23 24)))
(setq expected `((10 11 12 13 14)
		(20 21 @ 23 24)))
(unit-test (place-in-grid grid 2 1 `@) expected "5 x 2 grid, replace at (2, 1)")

(setq grid `((10 11 12 13 14)
	     (20 21 22 23 24)
	     (30 31 32 33 34)
	     (40 41 42 43 44)
	     (50 51 52 53 54)))
(setq expected `((@ 11 12 13 14)
		(20 21 22 23 24)
		(30 31 32 33 34)
		(40 41 42 43 44)
		(50 51 52 53 54)))
(unit-test (place-in-grid grid 0 0 `@) expected "5 x 5 grid, replace at (0, 0)")

(setq grid `((10 11 12 13 14)
	     (20 21 22 23 24)
	     (30 31 32 33 34)
	     (40 41 42 43 44)
	     (50 51 52 53 54)))
(setq expected `((10 11 12 13 14)
		(20 21 22 23 24)
		(30 31 @  33 34)
		(40 41 42 43 44)
		(50 51 52 53 54)))
(unit-test (place-in-grid grid 2 2 `@) expected "5 x 5 grid, replace at (2, 2)")

(setq grid `((10 11 12 13 14)
	     (20 21 22 23 24)
	     (30 31 32 33 34)
	     (40 41 42 43 44)
	     (50 51 52 53 54)))
(setq expected `((10 11 12 13 14)
		(20 21 22 23 24)
		(30 31 32 33 34)
		(40 41 42 43 44)
		(50 51 52 53 @ )))
(unit-test (place-in-grid grid 4 4 `@) expected "5 x 5 grid, replace at (4, 4)")