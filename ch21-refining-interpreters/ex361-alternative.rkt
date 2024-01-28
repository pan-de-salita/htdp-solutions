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

;;;; DATA DEF | BSL-VAR-EXPR -------------------------------

(define-struct fun-app [name arg] #:transparent)
;; a BSL-fun-app (short for function application) is a structure:
;;   (make-fun-app Symbol BSL-var-expr)
;; i.e. represents a function application, where (make-fun-app f x)
;; is equivalent to (f x) where a function f is applied to an expression f

(define fun-app0 (make-fun-app 'f (make-add 1 1)))
(define fun-app1 (make-mul 5 fun-app0))
(define fun-app2 (make-mul (make-fun-app 'g 5) fun-app0))

;; a BSL-fun-expr is one of:
;; - Number
;; - Symbol
;; - Add
;; - Mul
;; - BSL-fun-def

;;;; DATA DEF | BSL-FUN-DEF -------------------------------

(define-struct fun-def [name param body] #:transparent)
;; a BSL-fun-def (short for function definition) is a structure:
;;   (make-fun-def Symbol Symbol BSL-fun-expr)
;; i.e. represents a function definition, where (make-fun-def f x b)
;; is equivalent to (define (f x) b)

(define fun-def-f (make-fun-def 'f 'x (make-add 3 'x)))
(define fun-def-g (make-fun-def 'g 'y (make-fun-app 'f (make-mul 2 'y))))
(define fun-def-h (make-fun-def 'h 'v (make-add (make-fun-app 'f 'v)
                                                (make-fun-app 'g 'v))))

;; a BSL-fun-def* is a [List-of BSL-fun-def]
;; i.e. represents a definitions area that consists of a number
;; of one-argument function definitions

(define def-area-fgh `(,fun-def-f ,fun-def-g ,fun-def-h))

;;;; DATA-DEF | BSL-DA-ALL ---------------------------------

;; a BSL-da-all is a [List-of [Association or BSL-fun-def]]
;; i.e. represents a definitions area comprising of
;; constant and function definitions

(define close-to-pi
  '(close-to-pi 3.14))
(define fd-area-of-circle
  (make-fun-def 'area-of-circle
                'r
                (make-mul 'close-to-pi
                          (make-mul 'r 'r))))
(define fd-volume-of-10-cylinder
  (make-fun-def 'volume-of-10-cylinder
                'r
                (make-mul 10
                          (make-fun-app 'area-of-circle 'r))))

(define bda-pi (list close-to-pi fd-area-of-circle fd-volume-of-10-cylinder))

;;;; FUNCTIONS ---------------------------------------------

(define UNDEFINED-CONSTANT "error: undefined constant")
(define UNDEFINED-FUNCTION "error: undefined function")

;; BSL-fun-expr Symbol BSL-fun-expr -> BSL-fun-expr
;; substitutes all instances of param within body with a-bsl-fun-expr
(define (subst a-bsl-fun-expr parameter argument)
  (local (;; BSL-fun-expr -> BSL-fun-expr
          (define (plug-val-into expr)
            (subst expr parameter argument)))
    (match a-bsl-fun-expr
      [(? number?) a-bsl-fun-expr]
      [(? symbol?)
       (if (symbol=? a-bsl-fun-expr parameter)
           argument
           a-bsl-fun-expr)]
      [(add x y) (make-add (plug-val-into x) (plug-val-into y))]
      [(mul x y) (make-mul (plug-val-into x) (plug-val-into y))]
      [(fun-app name arg) (make-fun-app name (plug-val-into arg))])))

(check-expect (subst bsl-var-expr0 'x 5) 5)
(check-expect (subst bsl-var-expr1 'x 5) (make-add 5 3))
(check-expect (subst bsl-var-expr2 'x 5) (make-mul 1/2 (make-mul 5 3)))
(check-expect (subst bsl-var-expr3 'x 5) (make-add (make-mul 5 5)
                                                   (make-mul 'y 'y)))

;; BSL-da-all Symbol -> BSL-eval
;; returns the representation of a constant definition whose name is con,
;; if such data exists in def-area; otherwise, signals an error
(define (lookup-con-def def-area con-name)
  (local ((define lookup-con
            (filter (lambda (def)
                      (and (list? def) (symbol=? (car def) con-name)))
                    def-area)))
    (if (null? lookup-con)
        (error UNDEFINED-CONSTANT)
        (cadar lookup-con))))

(check-within (lookup-con-def bda-pi 'close-to-pi) 3.14 0.0001)
(check-error (lookup-con-def bda-pi 'area-of-circle) UNDEFINED-CONSTANT)

;; BSL-da-all Symbol -> BSL-fun-def
;; returns the representation of a function definition whose name is f-name,
;; if such data exists in def-area; otherwise, signals an error
(define (lookup-fun-def def-area f-name)
  (local ((define lookup-def
            (filter
             (lambda (def)
               (and (fun-def? def) (symbol=? (fun-def-name def) f-name)))
             def-area)))
    (if (empty? lookup-def)
        (error UNDEFINED-FUNCTION)
        (car lookup-def))))

(check-expect (lookup-fun-def bda-pi 'area-of-circle) fd-area-of-circle)
(check-expect (lookup-fun-def bda-pi 'volume-of-10-cylinder) fd-volume-of-10-cylinder)
(check-error (lookup-fun-def bda-pi 'pi) UNDEFINED-FUNCTION)

;; BSL-fun-expr BSL-da-all -> BSL-eval
;; returns the value of expr
(define (eval-all expr def-area)
  (local (;; BSL-fun-expr -> BSL-eval
          (define (evaluate x)
            (eval-all x def-area)))
    (match expr
      [(? number?) expr]
      [(? symbol?) (lookup-con-def def-area expr)]
      [(add x y) (+ (evaluate x) (evaluate y))]
      [(mul x y) (* (evaluate x) (evaluate y))]
      [(fun-app name arg)
       (local ((define value (evaluate arg))
               (define f (lookup-fun-def def-area name))
               (define plugd (subst (fun-def-body f)
                                    (fun-def-param f)
                                    value)))
         (evaluate plugd))])))

(check-within (eval-all (make-mul 3 'close-to-pi) bda-pi) 9.42 0.001)
(check-within (eval-all (make-fun-app 'area-of-circle 1) bda-pi) 3.14 0.001)
(check-within (eval-all (make-fun-app 'volume-of-10-cylinder 1) bda-pi) 31.4 0.001)
(check-within (eval-all (make-fun-app 'volume-of-10-cylinder 45) bda-pi) 63585.0 0.001)

(test)
