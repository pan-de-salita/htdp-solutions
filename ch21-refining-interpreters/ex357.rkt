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
;; a Fun-app (short for function application) is a structure:
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

;;;; FUNCTIONS ---------------------------------------------

(define UNDEFINED-VARIABLE/S "eval-variable: undefined variable given")
(define UNDEFINED-FUNCTION/S "error: undefined function(s) given")

;; BSL-fun-expr Symbol BSL-fun-expr -> BSL-fun-expr
;; substitutes all instances of param within body with a-bsl-fun-expr
(define (subst a-bsl-fun-expr parameter argument)
  (local (;; BSL-fun-expr -> BSL-fun-expr
          (define (plug-arg-into expr)
            (subst expr parameter argument)))
    (match a-bsl-fun-expr
      [(? number?) a-bsl-fun-expr]
      [(? symbol?)
       (if (symbol=? a-bsl-fun-expr parameter)
           argument
           a-bsl-fun-expr)]
      [(add x y) (make-add (plug-arg-into x) (plug-arg-into y))]
      [(mul x y) (make-mul (plug-arg-into x) (plug-arg-into y))]
      [(fun-app name arg) (make-fun-app name (plug-arg-into arg))])))

(check-expect (subst bsl-var-expr0 'x 5) 5)
(check-expect (subst bsl-var-expr1 'x 5) (make-add 5 3))
(check-expect (subst bsl-var-expr2 'x 5) (make-mul 1/2 (make-mul 5 3)))
(check-expect (subst bsl-var-expr3 'x 5) (make-add (make-mul 5 5)
                                                   (make-mul 'y 'y)))

;; Symbol Symbol BSL-fun-expr -> [BSL-fun-expr -> BSL-eval]
;; returns a function that determines the value of a-bsl-fun-expr
(define (eval-definition.v1 f-name f-param f-body)
  (lambda (a-bsl-fun-expr)
    (local (;; BSL-expr -> BSL-eval
            (define (f expr)
              ([eval-definition.v1 f-name f-param f-body] expr)))
      (match a-bsl-fun-expr
        [(? number?) a-bsl-fun-expr]
        [(? symbol?) (error UNDEFINED-VARIABLE/S)]
        [(add x y) (+ (f x) (f y))]
        [(mul x y) (* (f x) (f y))]
        [(fun-app name arg)
         (if (not (symbol=? name f-name))
             (error UNDEFINED-FUNCTION/S)
             (local ((define value (f arg))
                     (define plugd (subst f-body f-param value)))
               (f plugd)))]))))

(check-expect ([eval-definition.v1 'k 'x (make-mul 'x 'x)] fun-app0) 4)
(check-expect ([eval-definition.v1 'k 'x (make-mul 'x 'x)] fun-app1) 20)

(check-error ([eval-definition.v1 'i 'x (make-mul 'x 'x)] fun-app0) UNDEFINED-FUNCTION/S)
(check-error ([eval-definition.v1 'i 'x (make-mul 'x 'x)] fun-app1) UNDEFINED-FUNCTION/S)
(check-error ([eval-definition.v1 'k 'x (make-mul 'x 'x)] fun-app2) UNDEFINED-FUNCTION/S)
(check-error ([eval-definition.v1 'k 'x (make-mul 'x 'x)] bsl-var-expr0) UNDEFINED-VARIABLE/S)

;; NOTE
;; eval-dfinition.v1 does not terminate if f-body and a-bsl-fun-expr
;; are both fun-apps.
;;
;;   sample computation 1: ---------------------------------
;;
;;   ([eval-definition.v1 'k 'x fun-app0] fun-app0)
;;   ==
;;   [1] value == (f (make-add 1 1)) == 2
;;   [2] plugd == (subst (make-fun-app 'k (make-add 1 1)) 'x 2) == (make-fun-app 'k (make-add 1 1))
;;   [3] (f (make-fun-app 'k (make-add 1 1))) ;; which is where we first started
;;
;;   equivalent to:
;;   (define (k x) (k (+ 1 1)))
;;   (k (k + 1 1))
;;
;;   sample computation 2: ---------------------------------
;;
;;   ([eval-definition.v1 'k 'x (make-fun-app 'k (make-add 'x 'x))] fun-app0)
;;   ==
;;   [1] value == (f (make-add 1 1)) == 2
;;   [2] plugd == (subst (make-fun-app 'k (make-add 'x 'x)) 'x 2) == (make-fun-app 'k (make-add 2 2))
;;   [3] (f (make-fun-app 'k (make-add 2 2)))
;;   [4] value == (f (make-add 2 2)) == 4
;;   [5] plugd == (subst (make-fun-app 'k (make-add 'x 'x)) 'x 4) == (make-fun-app 'k (make-add 4 4))
;;   [6] (f (make-fun-app 'k (make-add 4 4)))
;;   [7] ... ;; note how a-bsl-fun-app only gets larger and larger
;;
;;   equivalent to:
;;   (define (k x) (k (+ x x)))
;;   (k (k + 1 1))

(test)
