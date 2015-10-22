(load `unit-testing.lsp)
(load `../src/general.lsp)

(format t "~%-- abs --~%")
(unit-test (abs 5) 5 "positive")
(unit-test (abs 0) 0 "zero")
(unit-test (abs -5) 5 "negative")

(format t "~%~%-- remove --~%")
(unit-test (remove `() 4) nil "list is nil")
(unit-test (remove `(0 1 2 3 4) nil) nil "value is nil")
(unit-test (remove `(0 1 2 3 4) 0) `(1 2 3 4) "remove value at beginning of list")
(unit-test (remove `(0 1 2 3 4) 2) `(0 1 3 4) "remove value at middle of list")
(unit-test (remove `(0 1 2 3 4) 4) `(0 1 2 3) "remove value at end of list")
(unit-test (remove `(0 1 4 3 4 5 4) 4) `(0 1 3 5) "list contains multiple instances of value")
(unit-test (remove `(0 1 2 3 4) 10) `(0 1 2 3 4) "does not contain value")

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

(format t "~%~%-- extract-list-value --~%")
(unit-test (extract-list-value `() 0) nil "list is nil")
(unit-test (extract-list-value `(1 2 3) nil) nil "value is nil")
(unit-test (extract-list-value `(1 2 3 4 5) 0) 1 "value at first index")
(unit-test (extract-list-value `(1 2 3 4 5) 2) 3 "value at middle index")
(unit-test (extract-list-value `(1 2 3 4 5) 4) 5 "value at end index")

(format t "~%~%-- occurrences-in-list --~%")
(unit-test (occurrences-in-list `() `@) 0 "list is nil")
(unit-test (occurrences-in-list `(1 2 3) `@) 0 "value is nil")
(unit-test (occurrences-in-list `(1 2 3 4 5) `@) 0 "not contained in list")
(unit-test (occurrences-in-list `(@ 2 3 4 5) `@) 1 "occurs at beggining of list")
(unit-test (occurrences-in-list `(1 2 3 4 @) `@) 1 "occurs at end of list")
(unit-test (occurrences-in-list `(@ 2 @ 4 @) `@) 3 "occurs throughout list")
(unit-test (occurrences-in-list `(@ @ @ @ @) `@) 5 "occurs in entire list")

(format t "~%~%-- place-in-list --~%")
(unit-test (place-in-list `() 0 9) nil "list is nil")
(unit-test (place-in-list `(1 2 3 4 5) 0 9) `(9 2 3 4 5) "place at start index")
(unit-test (place-in-list `(1 2 3 4 5) 2 9) `(1 2 9 4 5) "place at middle index")
(unit-test (place-in-list `(1 2 3 4 5) 4 9) `(1 2 3 4 9) "place at end index")

