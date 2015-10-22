(setq verbose t)
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