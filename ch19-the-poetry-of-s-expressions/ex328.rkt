#lang racket
(require test-engine/racket-tests)

;; S-expr Atom Atom -> S-expr
;; replaces all occurrences of atom-old with atom-new
;; in an s-expr

(define (substitute-sexpr sexpr old new)
  (match sexpr
    [(not (? pair?)) (if (equal? sexpr old) new sexpr)]
    [(? list?) (map (lambda (sl) (substitute-sexpr sl old new)) sexpr)]))

(check-expect (substitute-sexpr 0 0 1) 1)
(check-expect (substitute-sexpr 'a 0 1) 'a)
(check-expect (substitute-sexpr "a" 0 1) "a")
(check-expect (substitute-sexpr '() 0 1) '())
(check-expect (substitute-sexpr '(0 0 0) 0 1) '(1 1 1))
(check-expect (substitute-sexpr '((0) ((0) 0) (0 ((0)) 0 0)) 0 1) '((1) ((1) 1) (1 ((1)) 1 1)))

;; S-expr -> Number
;; determines the depth of an S-expr

(define (depth sexpr)
  (local (;; Atom -> Number
          (define (depth-atom atom) 1)
          ;; SL -> Number
          (define (depth-sl sl)
            (match sl
              [(? null?) 1] ;; an empty list has the depth of 1
              [(cons fst-sl rst-sl)
               (max ;; returns the largest depth given a collection of SL depths
                (add1 (depth fst-sl)) ;; calculates the depth of the first item in sl
                (depth-sl rst-sl))]))) ;; calculates the depth of each item in the rest of sl
    (match sexpr
      [(? atom?) (depth-atom sexpr)]
      [(? list?) (depth-sl sexpr)])))

(check-expect (depth 0) 1)
(check-expect (depth '()) 1)
(check-expect (depth '(())) 2)
(check-expect (depth '(1 2 3)) 2)
(check-expect (depth '((1) 2)) 3)
(check-expect (depth '(1 (2 ()) ((3)) (((4) 5)))) 5)
(check-expect (depth '(1 (2 ()) ((3 ((((((((4))))))))) 5 ((((((6) 7)))))))) 12)
(check-expect (depth '(() (()) ((())))) 4)

;; S-expr -> Number
;; like depth, determines the depth of an S-expr

(define (depth-simplified sexpr)
  (match sexpr
    [(? atom?) 1]
    [(? list?)
     (local ((define depth-of-null 1))
       (foldr max depth-of-null (map (lambda (s) (add1 (depth-simplified s))) sexpr)))]))

#|

NOTE: also works:

(define (depth-simplified sexpr)
  (match sexpr
    [(not (? pair?)) 1]
    [(? list?)
     (foldr max 0 (map (lambda (s) (add1 (depth-simplified s))) sexpr))]))

|#

(check-expect (depth-simplified 0) 1)
(check-expect (depth-simplified '()) 1)
(check-expect (depth-simplified '(1 2 3)) 2)
(check-expect (depth-simplified '(1 (2 ()) ((3)) (((4) 5)))) 5)
(check-expect (depth-simplified '(1 (2 ()) ((3 ((((((((4))))))))) 5 ((((((6) 7)))))))) 12)
(check-expect (depth-simplified '(() (()) ((())))) 4)

;; X -> Boolean
;; checks whether x is an Atom

(define (atom? x)
  (or (number? x) (string? x) (symbol? x)))

(check-expect (atom? 0) #t)
(check-expect (atom? "a") #t)
(check-expect (atom? 'a) #t)
(check-expect (atom? '()) #f)
(check-expect (atom? '(1 2 3)) #f)

;; S-expr -> Number
;; determines the depth of an S-expr

(define (depth-S8A sexpr)
  (local (;; Atom -> Number
          (define (depth-S8A-atom atom) 1)
          ;; SL -> SL
          (define (depth-S8A-sl sl)
            (cond
              [(empty? sl) 0]
              [else (max (depth-S8A (car sl))
                         (depth-S8A-sl (cdr sl)))])))
    (cond
      [(atom? sexpr) (depth-S8A-atom sexpr)]
      [(list? sexpr) (add1 (depth-S8A-sl sexpr))])))

(check-expect (depth-S8A 0) 1)
(check-expect (depth-S8A '()) 1)
(check-expect (depth-S8A '(1 2 3)) 2)
(check-expect (depth-S8A '((1) 2)) 3)
(check-expect (depth-S8A '(1 (2 ()) ((3)) (((4) 5)))) 5)
(check-expect (depth-S8A '(1 (2 ()) ((3 ((((((((4))))))))) 5 ((((((6) 7)))))))) 12)
(check-expect (depth-S8A '(() (()) ((())))) 4)

;; S-expr -> Number
;; determines the depth of an S-expr

(define (depth-S8A-simplified sexpr)
  (match sexpr
    [(? atom?) 1]
    [(? list?)
     (add1 (foldr max 0 (map depth-S8A-simplified sexpr)))]))

;; sample computations:
;; [1] input: '()
;; --- output: (add1 (foldr max 0 (map depth-S8A-simplified '())))
;; --- output: (add1 (foldr max 0 '()))
;; --- output: (add1 0)
;; --- output: 1
;; [2] input: '((1))
;; --- output: (add1 (foldr max 0 (map depth-S8A-simplified '((1)))))
;; --- output: (add1 (foldr max 0 (list (depth-S8A-simplified '(1)))))
;; --- output: (add1 (foldr max 0 (list (add1 (foldr max 0 (map depth-S8A-simplified '(1)))))))
;; --- output: (add1 (foldr max 0 (list (add1 (foldr max 0 (list (depth-S8A-simplified 1)))))))
;; --- output: (add1 (foldr max 0 (list (add1 (foldr max 0 (list 1))))))
;; --- output: (add1 (foldr max 0 (list (add1 1))))
;; --- output: (add1 (foldr max 0 (list 2)))
;; --- output: (add1 (max 2 0))
;; --- output: (add1 2)
;; --- output: 3
;; [3] input: '((1) 2)
;; --- output: (add1 (foldr max 0 (map depth-S8A-simplified '((1) 2))))
;; --- output: (add1 (foldr max 0 (list (depth-S8A-simplified '(1)) (depth-S8A-simplified 2))))
;; --- output: (add1 (foldr max 0 (list (add1 (foldr max 0 (map depth-S8A-simplified '(1)))) 1)))
;; --- output: (add1 (foldr max 0 (list (add1 (foldr max 0 (list (depth-S8A-simplified 1)))) 1)))
;; --- output: (add1 (foldr max 0 (list (add1 (foldr max 0 (list 1))) 1)))
;; --- output: (add1 (foldr max 0 (list (add1 (max 1 0)) 1)))
;; --- output: (add1 (foldr max 0 (list (add1 1) 1)))
;; --- output: (add1 (foldr max 0 (list 2 1)))
;; --- output: (add1 (max 2 1 0))
;; --- output: (add1 2)
;; --- output: 3
;; [4] input: '(1 2 3)
;; --- output: (add1 (foldr max 0 (map depth-S8A-simplified '(1 2 3))))
;; --- output: (add1 (foldr max 0 (list (depth-S8A-simplified 1) (depth-S8A-simplified 2) (depth-S8A-simplified 3))))
;; --- output: (add1 (foldr max 0 (list 1 1 1)))
;; --- output: (add1 (max 1 1 1 0))
;; --- output: (add1 1)
;; --- output: 2

(check-expect (depth-S8A-simplified 0) 1)
(check-expect (depth-S8A-simplified '()) 1)
(check-expect (depth-S8A-simplified '(1 2 3)) 2)
(check-expect (depth-S8A-simplified '(1 (2 ()) ((3)) (((4) 5)))) 5)
(check-expect (depth-S8A-simplified '(1 (2 ()) ((3 ((((((((4))))))))) 5 ((((((6) 7)))))))) 12)
(check-expect (depth-S8A-simplified '(() (()) ((())))) 4)

;; NOTE compare to:
;; S-expr -> Number
;; returns a false depth of an S-expr if sexpr is an SL.
;; false depth here arrived at by adding 1 to each comparison
;; between 2 depths within an SL using max
;;
;; failed attempt of simplifying S8A's solution

(define (depth-wrong sexpr)
  (local (;; Atom -> Number
          (define (depth-wrong-atom atom) 1)
          ;; SL -> SL
          (define (depth-wrong-sl sl)
            (cond
              [(empty? sl) 1]
              [else (add1 (max (depth-wrong (car sl))
                               (depth-wrong-sl (cdr sl))))])))
    (cond
      [(atom? sexpr) (depth-wrong-atom sexpr)]
      [(list? sexpr) (depth-wrong-sl sexpr)])))

;; S-expr -> Number
;; functions like depth-wrong
(define (depth-wrong-simplified sexpr)
  (cond
    [(atom? sexpr) 1]
    [else (foldr (lambda (x y) (add1 (max x y))) 1 (map depth-wrong-simplified sexpr))]))

(check-expect (depth-wrong '()) (depth-wrong-simplified '()))
(check-expect (depth-wrong '(1 2 3)) (depth-wrong-simplified '(1 2 3)))
(check-expect (depth-wrong '(1 (2 ()) ((3 ((((((((4))))))))) 5 ((((((6) 7))))))))
              (depth-wrong-simplified '(1 (2 ()) ((3 ((((((((4))))))))) 5 ((((((6) 7)))))))))

(test)
