(load `unit-testing.lsp)
(load `../src/grid.lsp)

(format t "~%-- extract-grid-value --~%")
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

(format t "~%~%-- occurrences-in-grid --~%")
(unit-test (occurrences-in-grid `() `@) 0 "grid is nil")
(unit-test (occurrences-in-grid `(1 2 3) `@) 0 "value is nil")

(setq grid `((00 10 20 30 40)
	     (01 11 21 31 41)
	     (02 12 22 32 42)
	     (03 13 23 33 43)
	     (04 14 24 34 44))
)
(unit-test (occurrences-in-grid grid `@) 0 "not contained in grid")

(setq grid `((@ 10 20 30 40)
	     (01 11 21 31 41)
	     (02 12 22 32 42)
	     (03 13 23 33 43)
	     (04 14 24 34 44))
)
(unit-test (occurrences-in-grid grid `@) 1 "at first")

(setq grid `((00 10 20 30 40)
	     (01 11 21 31 41)
	     (02 12 22 32 42)
	     (03 13 23 33 43)
	     (04 14 24 34 @))
)
(unit-test (occurrences-in-grid grid `@) 1 "at last index")

(setq grid `((@ 10 20 30 40)
	     (01 @ 21 31 41)
	     (02 @ 22 32 42)
	     (03 13 23 @ 43)
	     (04 @ 24 34 44))
)
(unit-test (occurrences-in-grid grid `@) 5 "throughout grid 1")

(setq grid `((@ 10 @ 30 40)
	     (01 @ 21 @ @)
	     (02 @ 22 32 42)
	     (03 13 @ 33 @)
	     (@ @ @ @ @))
)
(unit-test (occurrences-in-grid grid `@) 13 "throughout grid 2")

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
