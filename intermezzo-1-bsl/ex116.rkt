;; ex116
;; 1. x is syntactically legal because it is a value, and thus an expression (if predefined).
;; 2. (= y z) is syntactically legal because it is an expression of the form (primitive expr expr).
;; 3. (= (= y z) 0) is syntactiacally legal for similar reasons provided in 2. the difference is the
;;    (primitive expr expr) is nested within another one, where the former is treated as an expr,
;;    thereby giving us the form (primitive (primitive expr expr) expr).
;;
;; ex117
;; 1. (3 + 4) is illegal because it doesn't satisfy Racket's syntactical rule of using infix notation.
;;    it should instead read (+ 3 4).
;; 2. number? is illegal because it's a function that isn't applied to any value. it should be followed
;;    by some expression and then enclosed in parentheses, like so: (number? expr).
;; 3. (x) is illegal because it isn't a function yet is enclosed in parentheses. parentheses should only
;;    be used for primitive/function applications or conditional expressions. x would have been sufficient.
;;
;; ex118
;; 1. (define (f x) x) is legal because it satisfies the syntactical rule (define (variable variable ...) expr).
;; 2. (define (f x) y) is legal because of the same reason listed in 1.
;; 3. (define (f x y) 3) is legal because of the same reason listed in 1.
;;
;; ex119
;; 1. (define (f "x") x) is illegal because the form (define (variable value) expr) is used, while the keyword
;;    define only accepts (define (variable variable ...) expr).
;; 2. (define (f x y z) (x)) is illegal because the form (define (variable variable variable variable) function)
;;    is used, where the function isn't applied to any value. one acceptable way to rewrite the def is
;;    (define (f x y z) (x [expr ...])).
;;
;; ex120
;; 1. (x) is illegal for the reason listed in 3. of ex117.
;; 2. (+ 1 (not x)) is syntactically legal but would yield an error. since it satisfies the form (primitive expr expr),
;;    it is an expr.
;; 3. (+ 1 2 3) is legal. it's an expr because it satisfies the form (primitive expr expr).
;;
;; ex121
;; 1.
(+ (* (/ 12 8) 2/3)
      (- 20 (sqrt 4)))
;; == (+ (* (/ 12 8) 2/3) (- 20 2))
;; == (+ (* 1.5 2/3) (- 20 2))
;; == (+ (* 1.5 2/3) 18)
;; == (+ 1 18)
;; == 19

;; 2.
(cond
  [(= 0 0) #false]
  [(> 0 1) (string=? "a" "a")]
  [else (= (/ 1 0) 9)])
;; ==
;; (cond
;;   [#true #false]
;;   [(> 0 1) (string=? "a" "a")]
;;   [else (= (/ 1 0) 9)])
;; ==
;; (cond
;;   #false
;;   [(> 0 1) (string=? "a" "a")]
;;   [else (= (/ 1 0) 9)])
;; == #false

;; 3.
(cond
  [(= 2 0) #false]
  [(> 2 1) (string=? "a" "a")]
  [else (= (/ 1 2) 9)])
;; ==
;; (cond
;;  [#false #false]
;;  [(> 2 1) (string=? "a" "a")]
;;  [else (= (/ 1 2) 9)])
;; ==
;; (cond
;;  [(> 2 1) (string=? "a" "a")]
;;  [else (= (/ 1 2) 9)])
;; ==
;; (cond
;;  [#true (string=? "a" "a")]
;;  [else (= (/ 1 2) 9)])
;; ==
;; (cond
;;  [#true #true]
;;  [else (= (/ 1 2) 9)])
;; ==
;; #true
