#lang racket
(require test-engine/racket-tests)
(require lang/posn)

(define-struct ball [x y speed-x speed-y])

(number? (make-ball 1 2 3 4)) ;; -> #f
(ball-speed-y (make-ball (+ 1 2) (+ 3 3) 2 3)) ;; -> 3
(ball-y (make-ball (+ 1 2) (+ 3 3) 2 3)) ;; -> (+ 3 3) == 6
(ball-x (make-posn 1 2)) ;; -> error; expected (make-ball ...)
(ball-speed-y 5) ;; -> error; expected (make-ball ...)
