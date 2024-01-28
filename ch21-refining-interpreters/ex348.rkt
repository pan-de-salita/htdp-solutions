#lang racket
(require test-engine/racket-tests)

(define-struct && [left right] #:transparent)
;; an And is a structure:
;;   (make-&& Boolean Boolean)
;; i.e. represents the application of the logical
;; operator and to the Boolean values in its left
;; and right fields

(define-struct || [left right] #:transparent)
;; an Or is a structure:
;;   (make-|| Boolean Boolean)
;; i.e. represents the application of the logical
;; operator or to the Boolean values in its left
;; and right fields

(define-struct !! [value] #:transparent)
;; an Or is a structure:
;;   (make-! Boolean)
;; i.e. represents the application of the logical
;; operator or to a Boolean value

;; a Bool-BSL-expr is one of:
;; - Boolean
;; - And
;; - Or
;; - Not
(define bool-bsl-expr0 #t)
(define bool-bsl-expr1 #f)
(define bool-bsl-expr2 (make-&& #t #t))
(define bool-bsl-expr3 (make-&& #t #f))
(define bool-bsl-expr4 (make-|| #f #t))
(define bool-bsl-expr5 (make-|| #f #f))
(define bool-bsl-expr6 (make-&& (make-|| #t #t) #f))
(define bool-bsl-expr7 (make-!! (make-|| (make-|| #t #f) (make-&& #f #f))))

;; Bool-BSL-eval is a Boolean,
;; represents the value to which a Bool-BSL-expr evaluates
(define bool-bsl-eval0 #t)
(define bool-bsl-eval1 #f)
(define bool-bsl-eval2 #t)
(define bool-bsl-eval3 #f)
(define bool-bsl-eval4 #t)
(define bool-bsl-eval5 #f)
(define bool-bsl-eval6 #f)
(define bool-bsl-eval7 #f)

;; Bool-BSL-expr -> Bool-BSL-eval
;; returns the value of a Bool-BSL-expr
(define (eval-bool-expression a-bool-bsl-expr)
  (local (;; Boolean -> Bool-BSL-eval
          (define (eval-bool-expression-bool a-bool) a-bool)
          ;; And -> Bool-BSL-eval
          (define (eval-bool-expression-and an-and)
            (and (eval-bool-expression (&&-left an-and))
                 (eval-bool-expression (&&-right an-and))))
          ;; Or -> Bool-BSL-eval
          (define (eval-bool-expression-or an-or)
            (or (eval-bool-expression (||-left an-or))
                 (eval-bool-expression (||-right an-or))))
          ;; Not -> Bool-BSL-eval
          (define (eval-bool-expression-not a-not)
            (not (eval-bool-expression (!!-value a-not)))))
    (match a-bool-bsl-expr
      [(? boolean?) (eval-bool-expression-bool a-bool-bsl-expr)]
      [(? &&?) (eval-bool-expression-and a-bool-bsl-expr)]
      [(? ||?) (eval-bool-expression-or a-bool-bsl-expr)]
      [(? !!?) (eval-bool-expression-not a-bool-bsl-expr)])))

(check-expect (eval-bool-expression bool-bsl-expr0) bool-bsl-eval0)
(check-expect (eval-bool-expression bool-bsl-expr1) bool-bsl-eval1)
(check-expect (eval-bool-expression bool-bsl-expr2) bool-bsl-eval2)
(check-expect (eval-bool-expression bool-bsl-expr3) bool-bsl-eval3)
(check-expect (eval-bool-expression bool-bsl-expr4) bool-bsl-eval4)
(check-expect (eval-bool-expression bool-bsl-expr5) bool-bsl-eval5)
(check-expect (eval-bool-expression bool-bsl-expr6) bool-bsl-eval6)
(check-expect (eval-bool-expression bool-bsl-expr7) bool-bsl-eval7)

;; Bool-BSL-expr -> Bool-BSL-eval
;; returns the value of a Bool-BSL-expr
;; NOTE: .v2 of eval-bool-expression
(define (eval-bool-expression.v2 a-bool-bsl-expr)
  (match a-bool-bsl-expr
    [(? boolean?) a-bool-bsl-expr]
    [(&& x y) (and (eval-bool-expression.v2 x) (eval-bool-expression.v2 y))]
    [(|| x y) (or (eval-bool-expression.v2 x) (eval-bool-expression.v2 y))]
    [(!! expr) (not (eval-bool-expression.v2 expr))]))

(check-expect (eval-bool-expression.v2 bool-bsl-expr0) bool-bsl-eval0)
(check-expect (eval-bool-expression.v2 bool-bsl-expr1) bool-bsl-eval1)
(check-expect (eval-bool-expression.v2 bool-bsl-expr2) bool-bsl-eval2)
(check-expect (eval-bool-expression.v2 bool-bsl-expr3) bool-bsl-eval3)
(check-expect (eval-bool-expression.v2 bool-bsl-expr4) bool-bsl-eval4)
(check-expect (eval-bool-expression.v2 bool-bsl-expr5) bool-bsl-eval5)
(check-expect (eval-bool-expression.v2 bool-bsl-expr6) bool-bsl-eval6)
(check-expect (eval-bool-expression.v2 bool-bsl-expr7) bool-bsl-eval7)

;; Bool-BSL-expr -> Bool-BSL-eval
;; returns the value of a Bool-BSL-expr
;; NOTE: .v3 of eval-bool-expression
;; NOTE-NOTE: don't ship this lol (or maybe do?)
(define (eval-bool-expression.v3 a-bool-bsl-expr)
  (match a-bool-bsl-expr
    [(? boolean?) a-bool-bsl-expr]
    [(&& x y)
     (foldr (lambda (eval1 eval2) (and eval1 eval2))
            (eval-bool-expression.v3 y)
            `(,(eval-bool-expression.v3 x)))]
    [(|| x y)
     (foldr (lambda (eval1 eval2) (or eval1 eval2))
            (eval-bool-expression.v3 y)
            `(,(eval-bool-expression.v3 x)))]
    [(!! expr) (not (eval-bool-expression.v3 expr))]))

(check-expect (eval-bool-expression.v3 bool-bsl-expr0) bool-bsl-eval0)
(check-expect (eval-bool-expression.v3 bool-bsl-expr1) bool-bsl-eval1)
(check-expect (eval-bool-expression.v3 bool-bsl-expr2) bool-bsl-eval2)
(check-expect (eval-bool-expression.v3 bool-bsl-expr3) bool-bsl-eval3)
(check-expect (eval-bool-expression.v3 bool-bsl-expr4) bool-bsl-eval4)
(check-expect (eval-bool-expression.v3 bool-bsl-expr5) bool-bsl-eval5)
(check-expect (eval-bool-expression.v3 bool-bsl-expr6) bool-bsl-eval6)
(check-expect (eval-bool-expression.v3 bool-bsl-expr7) bool-bsl-eval7)

(test)
