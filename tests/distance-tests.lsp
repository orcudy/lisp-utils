(load `unit-testing.lsp)
(load `../src/distance.lsp)

(format t "~%-- closest-by-manhattan --~%")
(setq source `(1 1))
(setq destinations `((0 0)(1 1)(2 2)))
(unit-test (closest-by-manhattan source destinations) `((1 1) 0) "source: (1 1); destinations: ((0 0)(1 1)(2 2))")

(setq source `(10 10))
(setq destinations `((0 0)(1 1)(2 2)))
(unit-test (closest-by-manhattan source destinations) `((2 2) 16) "source: (10 10); destinations: ((0 0)(1 1)(2 2))")

(setq source `(-10 -10))
(setq destinations `((0 0)(1 1)(2 2)))
(unit-test (closest-by-manhattan source destinations) `((0 0) 20) "source: (-10 -10); destinations: ((0 0)(1 1)(2 2))")

(format t "~%~%-- manhattan-distance --~%")
(setq source `(0 0))
(setq destination `(0 0))
(unit-test (manhattan-distance source destination) 0 "source: (0, 0); destination: (0, 0)")

(setq source `(10 10))
(setq destination `(0 0))
(unit-test (manhattan-distance source destination) 20 "source: (10, 10); destination: (0, 0)")

(setq source `(0 0))
(setq destination `(-10 -10))
(unit-test (manhattan-distance source destination) 20 "source: (0, 0); destination: (-10, -10)")

(setq source `(10 10))
(setq destination `(-10 -10))
(unit-test (manhattan-distance source destination) 40 "source: (10, 10); destination: (-10, -10)")

