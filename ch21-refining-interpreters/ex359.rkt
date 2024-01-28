#lang racket
(require test-engine/racket-tests)

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
(define fun-app3
  ; => (+ (* 3 (f (* 2 5))) (+ (h (+ 2 (* 2 3))) (g (* 7 (+ 1 4)))))
  ; => (+ (* 3 (f 10)) (+ (h 8) (g 35)))
  (make-add (make-mul 3 (make-fun-app 'f (make-mul 2 5)))
            (make-add (make-fun-app 'h (make-add 2 (make-mul 2 3)))
                      (make-fun-app 'g (make-mul 7 (make-add 1 4))))))

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

;;;; FUNCTIONS ---------------------------------------------

(define UNDEFINED-VARIABLE/S "error: undefined variable(s) given")
(define UNDEFINED-FUNCTION/S "error: undefined function(s) given")

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

;; BSL-fun-def* Symbol -> BSL-fun-def
;; retrieves the definition of f in da;
;; signals an error if there is none
(define (lookup-def def-area f-name)
  (local ((define lookup-result
            (filter (lambda (f) (symbol=? (fun-def-name f) f-name)) def-area)))
    (if (null? lookup-result)
        (error UNDEFINED-FUNCTION/S)
        (car lookup-result))))

(check-expect (lookup-def def-area-fgh 'f) fun-def-f)
(check-expect (lookup-def def-area-fgh 'g) fun-def-g)
(check-expect (lookup-def def-area-fgh 'h) fun-def-h)

;; BSL-fun-expr BSL-fun-def* -> BSL-eval
;; determines the value of expr
(define (eval-definition.v2 expr def-area)
  (local (;; BSL-fun-def -> BSL-eval
          (define (evaluate ex)
            (eval-definition.v2 ex def-area)))
    (match expr
      [(? number?) expr]
      [(? symbol?) (error UNDEFINED-VARIABLE/S)]
      [(add x y) (+ (evaluate x) (evaluate y))]
      [(mul x y) (* (evaluate x) (evaluate y))]
      [(fun-app name arg)
       (local ((define value (evaluate arg))
               (define f (lookup-def def-area name))
               (define plugd (subst (fun-def-body f)
                                    (fun-def-param f)
                                    value)))
         (evaluate plugd))])))

(check-expect (eval-definition.v2 fun-app0 def-area-fgh) 5)
(check-expect (eval-definition.v2 fun-app1 def-area-fgh) 25)
(check-expect (eval-definition.v2 fun-app2 def-area-fgh) 65)
(check-expect (eval-definition.v2 fun-app3 def-area-fgh) 142)

;; taken from S8A's test suite:
(define kfuncbody (make-mul 3 'x))
(define ifuncbody (make-mul 'x (make-add 'x 1)))

(define fdk (make-fun-def 'f 'x kfuncbody))
(define fdi (make-fun-def 'g 'x ifuncbody))

(define da-ik (list fdi fdk))

(check-expect (eval-definition.v2 fun-app0 da-ik) 6)
(check-expect (eval-definition.v2 fun-app1 da-ik) 30)
(check-expect (eval-definition.v2 fun-app2 da-ik) 180)

(test)
