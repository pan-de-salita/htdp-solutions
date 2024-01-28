#lang racket
(require test-engine/racket-tests)

;; the interactions area performs the task of
;; determining the values of expressions that
;; you enter. it interprets the value of expressions
;; entered.

;;;; 21.1 interpreting expressions -------------------------

(define-struct add [left right] #:transparent)
;; an Add is a structure:
;;   (make-add Number Number)
;; i.e. the sum of two BSL expressions

(define-struct mul [left right] #:transparent)
;; an Add is a structure:
;;   (make-add Number Number)
;; i.e. the product of two BSL expressions

;; a BSL expression is one of:
;; - Number
;; - Add
;; - Mul

#|

[1]
bsl expression: 3
representation of bsl expression: 3
[2]
bsl expression: (+ 1 1)
representation of bsl expression: (make-add 1 1)
[3]
bsl expression: (* 1 1)
representation of bsl expression: (make-mul 1 1)
[4]
bsl expression: (+ (* 3 3) (* 4 4))
representation of bsl expression: (make-add (make-mul 3 3) (make-mul 4 4))

|#

;; NOTE: "interpreter" refers to a program that
;; consumes the representation of a program and
;; produces its value.

;; NOTE: a parser simultaneously checks whether
;; some piece of data conforms to a data definition,
;; and, if it does, builds a matching element from
;; the chosen class of data.

;; [Maybe Atom] -> Boolean
(define (atom? x)
  (ormap (lambda (test) (test x)) `(,number? ,string? ,symbol?)))

;; S-expr -> BSL-expr
(define (parse an-s-expr)
  (cond
    [(atom? an-s-expr) (parse-atom an-s-expr)]
    [(list? an-s-expr) (parse-sl an-s-expr)]))

;; Atom -> BSL-expr
(define (parse-atom an-atom)
  (match an-atom
    [(? number?) an-atom]
    [(? string?) (error "error")]
    [(? symbol?) (error "error")]))

;; SL -> BSL-expr
(define (parse-sl an-sl)
  (cond
    [(and (consists-of-3? an-sl) (symbol? (car an-sl)))
     (cond
       [(symbol=? (car an-sl) '+)
        (make-add (parse (cadr an-sl)) (parse (caddr an-sl)))]
       [(symbol=? (car an-sl) '*)
        (make-mul (parse (cadr an-sl)) (parse (caddr an-sl)))]
       [else (error "wrong")])]
    [else (error "wrong")]))

;; SL -> Boolean
(define (consists-of-3? an-sl)
  (and (cons? an-sl) (cons? (cdr an-sl)) (cons? (cdr (cdr an-sl)))
       (empty? (cdr (cdr (cdr an-sl))))))

;;;; 21.2 interpreting variables ---------------------------

;; NOTE: the preceding section implicity proposes symbols
;; as representatives for variables. after all, if you were
;; to choose quoted s-expressions to represent expressions
;; with variables, symbols would appear naturally:

;; a BSL-var-expr is one of:
;; -- Number
;; -- Symbol
;; -- (make-add BSL-var-expr BSL-var-expr)
;; -- (make-mul BSL-var-expr BSL-var expr)

;; an AL (short for association list) is [List-of Association].
;; an Associate is a list of two items:
;;   (cons Symbol (cons Number '())).

;;;; 21.3 interpreting functions ---------------------------

;; NOTE: since function definitions show up in the definitions
;; area, another way to describe the refined evaluator (the latest
;; iteration of which was eval-var-lookup) is to say that it
;; simulates DrRacket when the definitions area contains a number
;; of function definitons and a programmer enters an expression
;; in the interactions area that contains uses of these functions.

(define-struct fun [name arg])
;; a Fun (short for function) is a structure:
;;   (make-fun Symbol BSL-fun-expr)
;; representation of a function's name and argument
;; NOTE: assuming that said function accepts
;; only one argument

;; a BSL-fun-expr is one of:
;; - Number
;; - Symbol
;; - (make-add BSL-fun-expr BSL-fun-expr)
;; - (make-mul BSL-fun-expr BSL-fun-expr)
;; - (make-fun Symbol BSL-fun-expr)

#|

(k (+ 1 1)) == (make-fun 'k (make-add 1 1))
(* 5 (k (+ 1 1))) == (make-mul 5 (make-fun 'k (make-add 1 1)))
(* (i 5) (k (+ 1 1))) == (make-mul (make-fun 'i 5) (make-fun 'k (make-add 1 1)))

|#



