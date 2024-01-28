#lang racket
(require test-engine/racket-tests)

(define-struct add [left right])
;; an Add is a structure:
;;   (make-add Number Number)
;; i.e. the sum of two BSL expressions

(define-struct mul [left right])
;; a Mul is a structure:
;;   (make-add Number Number)
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

;; BSL-expr -> BSL-eval
;; computes the value of a BSL-expr
(define (eval-expression bsl-expr)
  (local (;; Number -> BSL-eval
          (define (eval-num num) num)
          ;; Add -> BSL-eval
          (define (eval-add add)
            (+ (eval-expression (add-left add))
               (eval-expression (add-right add))))
          ;; Mul -> BSL-eval
          (define (eval-mul add)
            (* (eval-expression (mul-left add))
               (eval-expression (mul-right add)))))
    (match bsl-expr
      [(? number?) (eval-num bsl-expr)]
      [(? add?) (eval-add bsl-expr)]
      [(? mul?) (eval-mul bsl-expr)])))

(check-expect (eval-expression bsl-expr0) bsl-eval0)
(check-expect (eval-expression bsl-expr1) bsl-eval1)
(check-expect (eval-expression bsl-expr2) bsl-eval2)
(check-expect (eval-expression bsl-expr3) bsl-eval3)

;; BSL-expr -> BSL-eval
;; .v2 of eval-expression
;; computes the value of a BSL-expr
(define (eval-expression.v2 bsl-expr)
  (match bsl-expr
    [(? number?) bsl-expr]
    [(add x y) (+ (eval-expression x) (eval-expression y))]
    [(mul x y) (* (eval-expression x) (eval-expression y))]))

(check-expect (eval-expression.v2 bsl-expr0) bsl-eval0)
(check-expect (eval-expression.v2 bsl-expr1) bsl-eval1)
(check-expect (eval-expression.v2 bsl-expr2) bsl-eval2)
(check-expect (eval-expression.v2 bsl-expr3) bsl-eval3)

(test)
