#lang racket
(require test-engine/racket-tests)

;; an S-expr is one of:
;; - Atom, a data type x that satisfies
;;   the predicate (not (pair? x))
;; - SL, [List-of S-expr]
;; examples:
(define SEXP0 -1)
(define SEXP1 "zero")
(define SEXP2 'one)
(define SEXP3 '())
(define SEXP4 (list SEXP0 SEXP1 SEXP2))
(define SEXP5 (list SEXP4))
(define SEXP6 (list SEXP4
                    (list SEXP4
                          (list 'one))))

;; S-expr Atom -> Number
;; counts the occurrences of atom in sexpr

(define (count.v1 sexpr atom)
  (local (;; SL -> Number
          (define (count.v1-sl sl)
            (cond
              [(null? sl) 0]
              [else (+ ;; sums total count so far | let's call this step [0]
                     (count.v1 (car sl) atom) ;; sums total count within (car sl) | let's call step [1]
                     (count.v1-sl (cdr sl)))]))) ;; sums total count within (cdr sl) | let's call step [2]
    (match sexpr
      [(not (? pair?)) (if (equal? sexpr atom) 1 0)]
      [(? list?) (count.v1-sl sexpr)])))

(check-expect (count.v1 SEXP0 'one) 0)
(check-expect (count.v1 SEXP1 'one) 0)
(check-expect (count.v1 SEXP2 'one) 1)
(check-expect (count.v1 SEXP3 'one) 0)
(check-expect (count.v1 SEXP4 'one) 1)
(check-expect (count.v1 SEXP5 'one) 1)
(check-expect (count.v1 SEXP6 'one) 3)

;; S-expr Atom -> Number
;; counts the occurrences of atom in sexpr

(define (count.v2 sexpr atom)
  (match sexpr
    [(not (? pair?)) (if (equal? sexpr atom) 1 0)]
    [(? list?)
     (foldr
      (lambda (from-sexpr total-count)
        (+ ;; [0] abstracted
         (count.v2 from-sexpr atom) ;; [1] abstracted
         total-count)) ;; [2] abstracted
      0
      sexpr)]))
      ;; the foldr expression used above is closer to the
      ;; shape of the data definition of S-expr than:
      ;;   (foldr (lambda (x y) (+ x y)) 0 (map (curryr count.v2 sym) sexpr))

(check-expect (count.v2 SEXP0 'one) (count.v1 SEXP0 'one))
(check-expect (count.v2 SEXP1 'one) (count.v1 SEXP1 'one))
(check-expect (count.v2 SEXP2 'one) (count.v1 SEXP2 'one))
(check-expect (count.v2 SEXP3 'one) (count.v1 SEXP3 'one))
(check-expect (count.v2 SEXP4 'one) (count.v1 SEXP4 'one))
(check-expect (count.v2 SEXP5 'one) (count.v1 SEXP5 'one))
(check-expect (count.v2 SEXP6 'one) (count.v1 SEXP6 'one))

(test)
