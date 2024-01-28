#lang racket
(require test-engine/racket-tests)

;; [Number -> Boolean]
(check-expect (even?* 1) #f)
(check-expect (even?* 2) #t)

(define (even?* x) (= (modulo x 2) 0))

;; [Boolean String -> Boolean]
(check-expect (opposite/bool #t "opposite") #f)
(check-expect (opposite/bool #f "opposite") #t)
(check-expect (opposite/bool #f "reverse") #f)

(define (opposite/bool bool str)
  (cond [(string=? str "opposite") (if (equal? bool #t) #f #t)]
        [else bool]))

;; [Number Number Number -> Number]
(check-expect (add-3-nums 1 2 3) 6)
(check-expect (add-3-nums 4 5 6) 15)

(define (add-3-nums x y z) (+ x y z))

;; [Number -> [List-of-Number]]
(check-expect (countdown 0) '())
(check-expect (countdown 3) (list 3 2 1))
(check-expect (countdown -3) (list 3 2 1))

(define (countdown x) (if (= x 0) '() (cons (abs x) (countdown (sub1 (abs x))))))

;; [[List-of Number] -> Boolean]
(check-expect (even?/num-list (list 1 2 3)) #f)
(check-expect (even?/num-list (list 2 4 6)) #t)

(define (even?/num-list l-n)
  (cond [(empty? l-n) #t]
        [else (and (even?* (car l-n)) (even?/num-list (cdr l-n)))]))

(test)
