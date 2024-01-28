#lang racket
(require test-engine/racket-tests)

;; an S-expr is one of:
;; - Atom
;; - [List-of S-expr]

;; an Atom is one of:
;; - Number
;; - String
;; - Symbol

(define-struct add [left right] #:transparent)
;; an Add is a structure:
;;   (make-add Number Number)
;; i.e. the sum of two BSL expressions

(define-struct mul [left right] #:transparent)
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

;; [Maybe Atom] -> Boolean
;; checks if x is an Atom
(define (atom? x)
  (ormap (lambda (test) (test x)) `(,number? ,string? ,symbol?)))

(check-expect (atom? 0) #t)
(check-expect (atom? "a") #t)
(check-expect (atom? 'a) #t)

;; S-expr -> BSL-expr
;; if an-s-expr is the result of quoting a BSL expression,
;; produces its BSL-expr representative
(define (parse an-s-expr)
  (cond
    [(atom? an-s-expr) (parse-atom an-s-expr)]
    [(list? an-s-expr) (parse-sl an-s-expr)]))

(check-expect (parse 3) 3)
(check-expect (parse '(+ 1 1)) bsl-expr1)
(check-expect (parse '(* 3 10)) bsl-expr2)
(check-expect (parse '(+ (* 1 1) 10)) bsl-expr3)
(check-error (parse "a") "error")
(check-error (parse 'a) "error")

;; Atom -> BSL-expr
;; produces the BSL-expr representative of a Number
(define (parse-atom an-atom)
  (match an-atom
    [(? number?) an-atom]
    [(? string?) (error "error")]
    [(? symbol?) (error "error")]))

(check-expect (parse-atom 0) 0)
(check-error (parse-atom "a") "error")
(check-error (parse-atom 'a) "error")

;; SL -> BSL-expr
;; produces the BSL-expr representative of an SL
;; if it has the following shapes:
;;   (list '+ Number Number)
;;   (list '* Number Number)
;; NOTE: does not follow the data template of an SL
;; ('() or (cons S-expr SL)), which warrants a cond
;; for an empty list and one for a non-empty list.
;; instead, the program first tests for the length
;; and the value of the first element of an SL, and
;; follows a number of logical branches therefrom.
;; a redefinition of an acceptable SL for this function
;; might help us stick closer to the design recipe.
(define (parse-sl an-sl)
  (cond
    [(and (consists-of-3? an-sl) (symbol? (car an-sl)))
     (cond
       [(symbol=? (car an-sl) '+)
        (make-add (parse (cadr an-sl)) (parse (caddr an-sl)))]
       [(symbol=? (car an-sl) '*)
        (make-mul (parse (cadr an-sl)) (parse (caddr an-sl)))]
       [else (error "error")])]
    [else (error "error")]))

(check-expect (parse-sl '(+ 1 1)) (make-add (parse 1) (parse 1)))
(check-expect (parse-sl '(* 3 10)) (make-mul (parse 3) (parse 10)))
(check-expect (parse-sl '(+ (* 1 1) 10)) (make-add (parse '(* 1 1)) (parse 10)))
(check-error (parse-sl '(+ 1)) "error")
(check-error (parse-sl '(1 + 1)) "error")
(check-error (parse-sl '(- 5 1)) "error")
(check-error (parse-sl '()) "error")
(check-error (parse-sl '(+ () ())) "error")

;; SL -> BSL-expr
;; produces the BSL-expr representative of an SL
;; if it has the following shapes:
;;   (list '+ Number Number)
;;   (list '* Number Number)
(define (parse-sl.v2 an-sl)
  (cond
    [(not
      (and (= (length an-sl) 3)
           (symbol? (car an-sl))))
     (error "error")]
    [else
     (cond
       [(symbol=? (car an-sl) '+)
        (make-add (parse (cadr an-sl)) (parse (caddr an-sl)))]
       [(symbol=? (car an-sl) '*)
        (make-mul (parse (cadr an-sl)) (parse (caddr an-sl)))]
       [else (error "error")])]))

(check-expect (parse-sl.v2 '(+ 1 1)) (make-add (parse 1) (parse 1)))
(check-expect (parse-sl.v2 '(* 3 10)) (make-mul (parse 3) (parse 10)))
(check-expect (parse-sl.v2 '(+ (* 1 1) 10)) (make-add (parse '(* 1 1)) (parse 10)))
(check-error (parse-sl.v2 '(+ 1)) "error")
(check-error (parse-sl.v2 '(1 + 1)) "error")
(check-error (parse-sl.v2 '(- 5 1)) "error")
(check-error (parse-sl.v2 '()) "error")
(check-error (parse-sl.v2 '(+ () ())) "error")

;; SL -> Boolean
;; checks if an SL contains exactly 3 non-null elements
(define (consists-of-3? an-sl)
  (and (cons? an-sl) (cons? (cdr an-sl)) (cons? (cdr (cdr an-sl)))
       (empty? (cdr (cdr (cdr an-sl))))))

(check-expect (consists-of-3? '(+ 1 1)) #t)
(check-expect (consists-of-3? '(* 3 10)) #t)
(check-expect (consists-of-3? '(+ (* 1 1) 10)) #t)
(check-expect (consists-of-3? '(+ 1)) #f)
(check-expect (consists-of-3? '(1 + 1)) #t)
(check-expect (consists-of-3? '(- 5 1)) #t)
(check-expect (consists-of-3? '()) #f)
(check-expect (consists-of-3? '(+ () ())) #t)

(test)
