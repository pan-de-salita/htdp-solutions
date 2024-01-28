#lang racket
(require test-engine/racket-tests)

;;;; DATA DEF | S-EXPR -------------------------------------

;; an S-expr is one of:
;; - Atom
;; - SL

;; an Atom is one of:
;; - Number
;; - String
;; - Symbol

;; an SL is one of:
;; - '()
;; - (cons S-expr SL)

(define s-expr0 3)
(define s-expr1 "a")
(define s-expr2 'a)
(define s-expr3 '())
(define s-expr4 '(0 1 2))
(define s-expr5 '("a" 2 c))
(define s-expr6 '(+ 1 1))
(define s-expr7 '(* 3 10))
(define s-expr8 '(+ (* 1 1) 10))

;;;; DATA DEF | BSL-EXPR -----------------------------------

(define-struct add [left right] #:transparent)
;; an Add is a structure:
;;   (make-add BSL-expr BSL-expr)
;; i.e. the sum of two BSL expressions

(define-struct mul [left right] #:transparent)
;; a Mul is a structure:
;;   (make-add BSL-expr BSL-expr)
;; i.e. the product of two BSL expressions

;; a BSL-expr is one of:
;; - Number
;; - Add
;; - Mul
(define bsl-expr0 3)
(define bsl-expr1 (make-add 1 1))
(define bsl-expr2 (make-mul 3 10))
(define bsl-expr3 (make-add (make-mul 1 1) 10))

;; a BSL-eval is a Number
;; i.e. the class of values to which a BSL-expr evaluates
(define bsl-eval0 3)
(define bsl-eval1 (+ 1 1))
(define bsl-eval2 (* 3 10))
(define bsl-eval3 (+ (* 1 1) 10))

;;;; DATA DEF | BSL-VAR-EXPR -------------------------------

;; a BSL-var-expr is one of:
;; - Number
;; - Symbol
;; - (make-add BSL-var-expr BSL-var-expr)
;; - (make-add BSL-var-expr BSL-var-expr)

(define bsl-var-expr0 'x)
(define bsl-var-expr1 (make-add 'x 3))
(define bsl-var-expr2 (make-mul 1/2 (make-mul 'x 3)))
(define bsl-var-expr3 (make-add (make-mul 'x 'x)
                                (make-mul 'y 'y)))

;; NOTE: x == 5 and y == 3

;;;; DATA | AL ---------------------------------------------

;; an AL (short for association list) is [List-of Association].
;; an Association is a list of two items:
;;   (cons Symbol (cons Number '())).

(define assoc0 '(x 5))
(define assoc1 '(y 3))
(define assoc2 '(z 1))

(define al0 '())
(define al1 `(,assoc0))
(define al2 `(,assoc0 ,assoc1 ,assoc2))

;;;; DATA DEF | BSL-FUN-EXPR -------------------------------

(define-struct fun-app [name arg] #:transparent)
;; a Fun-App (short for function application) is a structure:
;;   (make-fun-app Symbol BSL-var-expr)
;; i.e. represents a function application, where (make-fun-app f x)
;; is equivalent to (f x) where a function f is applied to an expression f

(define fun-app0 (make-fun-app 'k (make-add 1 1)))
(define fun-app1 (make-mul 5 fun-app0))
(define fun-app2 (make-mul (make-fun-app 'i 5) fun-app0))

;; a BSL-fun-expr is one of:
;; - Number
;; - Symbol
;; - (make-add BSL-fun-expr BSL-fun-expr)
;; - (make-add BSL-fun-expr BSL-fun-expr)
;; - (make-fun-app BSL-fun-expr BSL-fun-expr)
