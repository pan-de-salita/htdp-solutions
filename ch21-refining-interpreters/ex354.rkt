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

;;;; DATA DEF | PSE ----------------------------------------

;; a PSE (Parsable S-expression) is one of:
;; - Parsable-Atom
;; - Parsable-SL

;; a PA (short for Parsable Atom) is a Number
;; i.e. a Number that can be represented as a BSL-expr

;; a PSL (short for Parsable-SL) is one of:
;; - (list '+ PSE PSE)
;; - (list '* PSE PSE)
;; i.e. an SL that is the result of quoting a BSL expression

(define pse0 3) ;; == s-expr0
(define pse1 '(+ 1 1)) ;; == s-expr6
(define pse2 '(* 3 10)) ;; ==  s-expr7
(define pse3 '(+ (* 1 1) 10)) ;; == s-expr8

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

;;;; FUNCTIONS ---------------------------------------------

(define NON-PARSABLE "parse: non-parsable data given")
(define UNDEFINED-VARIABLE/S "eval-variable: undefined variable given")

;; BSL-var-expr Symbol Number -> BSL-expr
;; replaces all instances of x in a-bsl-var-expr with v,
;; thus producing a BSL-expr
(define (subst a-bsl-var-expr x v)
  (match a-bsl-var-expr
    [(? number?) a-bsl-var-expr]
    [(? symbol?) (if (symbol=? a-bsl-var-expr x) v a-bsl-var-expr)]
    [(add left right) (make-add (subst left x v) (subst right x v))]
    [(mul left right) (make-mul (subst left x v) (subst right x v))]))

(check-expect (subst bsl-var-expr0 'x 5) 5)
(check-expect (subst bsl-var-expr1 'x 5) (make-add 5 3))
(check-expect (subst bsl-var-expr2 'x 5) (make-mul 1/2 (make-mul 5 3)))
(check-expect (subst bsl-var-expr3 'x 5) (make-add (make-mul 5 5)
                                                   (make-mul 'y 'y)))

;; BSL-var-expr -> Boolean
;; checks whether a-bsl-var-expr is also a bsl-expr
(define (numeric? a-bsl-var-expr)
  (match a-bsl-var-expr
    [(? number?) #t]
    [(? symbol?) #f]
    [(add left right) (and (numeric? left) (numeric? right))]
    [(mul left right) (and (numeric? left) (numeric? right))]))

(check-expect (numeric? bsl-var-expr0) #f)
(check-expect (numeric? bsl-var-expr1) #f)
(check-expect (numeric? bsl-var-expr2) #f)
(check-expect (numeric? bsl-var-expr3) #f)
(check-expect (numeric? (subst bsl-var-expr0 'x 5)) #t)
(check-expect (numeric? (subst bsl-var-expr1 'x 5)) #t)
(check-expect (numeric? (subst bsl-var-expr2 'x 5)) #t)
(check-expect (numeric? (subst bsl-var-expr3 'x 5)) #f)

;; BSL-var-expr -> BSL-eval
;; if a-bsl-var-expr is also a BSL-expr, returns the value
;; of the input
(define (eval-variable a-bsl-var-expr)
  (if (not (numeric? a-bsl-var-expr))
      (error UNDEFINED-VARIABLE/S)
      (eval-expression a-bsl-var-expr)))

(check-expect (eval-variable (subst bsl-var-expr0 'x 5)) 5)
(check-expect (eval-variable (subst bsl-var-expr1 'x 5)) 8)
(check-within (eval-variable (subst bsl-var-expr2 'x 5)) 7.5 0.001)

(check-error (eval-variable (subst bsl-var-expr3 'x 5)) UNDEFINED-VARIABLE/S)

;; BSL-var-expr AL -> BSL-eval
;; returns the value of a-bsl-var-expr
(define (eval-variable* a-bsl-var-expr def-area)
  (local (;; BSL-var-expr AL -> [Maybe BSL-expr]
          (define (apply-associations bsl-var l-assoc)
            (cond
              [(null? (cdr l-assoc))
               (subst bsl-var (caar l-assoc) (cadar l-assoc))]
              [else (apply-associations
                     (subst bsl-var (caar l-assoc) (cadar l-assoc))
                     (cdr l-assoc))]))
          (define associations-applied
            (apply-associations a-bsl-var-expr def-area)))
    (eval-variable associations-applied)))

(check-expect (eval-variable* bsl-var-expr0 al2) 5)
(check-expect (eval-variable* bsl-var-expr1 al2) 8)
(check-within (eval-variable* bsl-var-expr2 al2) 7.5 0.001)
(check-expect (eval-variable* bsl-var-expr3 al2) 34)

;; BSL-var-expr AL -> BSL-eval
;; returns the value of a-bsl-var-expr
;; NOTE: .v2 of eval-variable*
(define (eval-variable*.v2 a-bsl-var-expr def-area)
  (local (;; BSL-var-expr AL -> [Maybe BSL-expr]
          (define (apply-associations bsl-var l-assoc)
            (foldr (lambda (an-assoc bsl-expr)
                     (subst bsl-expr (car an-assoc) (cadr an-assoc)))
                   bsl-var
                   l-assoc))
          (define associations-applied
            (apply-associations a-bsl-var-expr def-area)))
    (eval-variable associations-applied)))

(check-expect (eval-variable*.v2 bsl-var-expr0 al2) 5)
(check-expect (eval-variable*.v2 bsl-var-expr1 al2) 8)
(check-within (eval-variable*.v2 bsl-var-expr2 al2) 7.5 0.001)
(check-expect (eval-variable*.v2 bsl-var-expr3 al2) 34)

;; X -> Boolean
;; checks if x is an Atom
(define (atom? x)
  (ormap (lambda (test) (test x)) `(,number? ,string? ,symbol?)))

(check-expect (atom? 0) #t)
(check-expect (atom? "a") #t)
(check-expect (atom? 'a) #t)
(check-expect (atom? #t) #f)
(check-expect (atom? '()) #f)
(check-expect (atom? '(1 2 3)) #f)

;; S-expr -> BSL-expr
;; returns a BSL-expr representation of an-s-expr
;; if it is parsable
(define (parse an-s-expr)
  (local (;; Atom -> BSL-expr
          (define (parse-atom an-atom)
            (local (;; Atom -> Boolean
                    (define (parsable-atom? x)
                      (number? x)))
              (match an-atom
                [(? parsable-atom?) an-atom]
                [_ (error NON-PARSABLE)])))
          ;; SL -> BSL-expr
          (define (parse-sl an-sl)
            (local (;; SL -> Boolean
                    (define (parsable-sl? maybe-psl)
                      (and (= (length maybe-psl) 3)
                           (symbol? (car maybe-psl)))))
              (match an-sl
                [(? parsable-sl?)
                 (local (;; PSL -> BSL-expr
                         (define (psl->bsl-expr psl)
                           (match psl
                             [(list '+ left right)
                              (make-add (parse left) (parse right))]
                             [(list '* left right)
                              (make-mul (parse left) (parse right))]
                             [_ (error NON-PARSABLE)])))
                   (psl->bsl-expr an-sl))]
                [_ (error NON-PARSABLE)]))))
    (match an-s-expr
      [(? atom?) (parse-atom an-s-expr)]
      [_ (parse-sl an-s-expr)])))

(check-expect (parse pse0) bsl-expr0)
(check-expect (parse pse1) bsl-expr1)
(check-expect (parse pse2) bsl-expr2)
(check-expect (parse pse3) bsl-expr3)

(check-error (parse s-expr1) NON-PARSABLE)
(check-error (parse s-expr2) NON-PARSABLE)
(check-error (parse s-expr3) NON-PARSABLE)
(check-error (parse s-expr4) NON-PARSABLE)
(check-error (parse s-expr5) NON-PARSABLE)

;; BSL-expr -> BSL-eval
;; returns a BSL-expr as a BSL-eval
(define (eval-expression a-bsl-expr)
  (match a-bsl-expr
    [(? number?) a-bsl-expr]
    [(add x y) (+ (eval-expression x) (eval-expression y))]
    [(mul x y) (* (eval-expression x) (eval-expression y))]))

(check-expect (eval-expression bsl-expr0) bsl-eval0)
(check-expect (eval-expression bsl-expr1) bsl-eval1)
(check-expect (eval-expression bsl-expr2) bsl-eval2)
(check-expect (eval-expression bsl-expr3) bsl-eval3)

;; S-expr -> BSL-eval
;; returns the value of an-s-expr if it is a PSE
(define (interpreter-expr an-s-expr)
  (eval-expression (parse an-s-expr)))

(check-expect (interpreter-expr pse0) (eval-expression bsl-eval0))
(check-expect (interpreter-expr pse1) (eval-expression bsl-eval1))
(check-expect (interpreter-expr pse2) (eval-expression bsl-eval2))
(check-expect (interpreter-expr pse3) (eval-expression bsl-eval3))

(check-error (interpreter-expr s-expr1) NON-PARSABLE)
(check-error (interpreter-expr s-expr2) NON-PARSABLE)
(check-error (interpreter-expr s-expr3) NON-PARSABLE)
(check-error (interpreter-expr s-expr4) NON-PARSABLE)
(check-error (interpreter-expr s-expr5) NON-PARSABLE)

(test)
