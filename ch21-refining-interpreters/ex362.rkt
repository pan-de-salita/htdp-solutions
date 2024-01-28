#lang racket
(require test-engine/racket-tests)

;;;; DATA DEFINITIONS --------------------------------------

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

;; a BSL-fun-expr is one of:
;; - Number
;; - Symbol
;; - Add
;; - Mul
;; - BSL-fun-def

(define-struct add [left right] #:transparent)
;; an Add is a structure:
;;   (make-add BSL-fun-expr BSL-fun-expr)
;; i.e. the sum of two BSL expressions

(define-struct mul [left right] #:transparent)
;; a Mul is a structure:
;;   (make-add BSL-fun-expr BSL-fun-expr)
;; i.e. the product of two BSL expressions

(define-struct fun-app [name arg] #:transparent)
;; a BSL-fun-app (short for function application) is a structure:
;;   (make-fun-app Symbol BSL-fun-expr)
;; i.e. represents a function application, where (make-fun-app f x)
;; is equivalent to (f x) where a function f is applied to an expression f

(define-struct const-def [name value] #:transparent)
;; a BSL-const-def (short for constant definition) is a structure:
;;   (make-fun-def Symbol Number)
;; i.e. represents a constant definition,
;; where (make-const-def name value) is equivalent to (define name value)

(define-struct fun-def [name param body] #:transparent)
;; a BSL-fun-def (short for function definition) is a structure:
;;   (make-fun-def Symbol Symbol BSL-fun-expr)
;; i.e. represents a one-argument function definition,
;; where (make-fun-def name param body) is equivalent to (define (name param) body)

;; a BSL-da (short for BSL definitions area) is one of:
;; - BSL-const-def
;; - BSL-fun-def

;; a BSL-da-all is a [List-of BSL-da]
;; i.e. represents a definitions area comprising of
;; constant and function definitions

;; a BSL-eval is a Number
;; i.e. the class of values to which a BSL-fun-expr evaluates

;;;; DATA EXAMPLES -----------------------------------------

(define sexpr0 3)
(define sexpr1 '(+ 1 1))
(define sexpr2 '(* 3 10))
(define sexpr3 '(+ (* 1 1) 10))
(define sexpr4 'x)
(define sexpr5 '(+ x 3))
(define sexpr6 '(* 1/2 (* x 3)))
(define sexpr7 '(+ (* x x) (* y y)))
(define sexpr8 '(f (+ 1 1)))
(define sexpr9 '(* 5 (f (+ 1 1))))
(define sexpr10 '(* (g 5) (f (+ 1 1))))
(define sexpr11 '(* (g (h x)) (f (+ 1 1))))

(define bsl-fun-expr0 3)
(define bsl-fun-expr1 (make-add 1 1))
(define bsl-fun-expr2 (make-mul 3 10))
(define bsl-fun-expr3 (make-add (make-mul 1 1) 10))
(define bsl-fun-expr4 'x)
(define bsl-fun-expr5 (make-add 'x 3))
(define bsl-fun-expr6 (make-mul 1/2 (make-mul 'x 3)))
(define bsl-fun-expr7 (make-add (make-mul 'x 'x) (make-mul 'y 'y)))
(define bsl-fun-expr8 (make-fun-app 'f (make-add 1 1)))
(define bsl-fun-expr9 (make-mul 5 bsl-fun-expr8))
(define bsl-fun-expr10 (make-mul (make-fun-app 'g 5) bsl-fun-expr8))
(define bsl-fun-expr11 (make-mul (make-fun-app 'g (make-fun-app 'h 'x)) bsl-fun-expr8))

(define cons-def0 (make-const-def 'x 5))
(define cons-def1 (make-const-def 'y 3))
(define cons-def2 (make-const-def 'z 1))

(define fun-def-f (make-fun-def 'f 'x (make-add 3 'x)))
(define fun-def-g (make-fun-def 'g 'y (make-fun-app 'f (make-mul 2 'y))))
(define fun-def-h (make-fun-def 'h 'v (make-add (make-fun-app 'f 'v) (make-fun-app 'g 'v))))
(define def-area-fgh `(,fun-def-f ,fun-def-g ,fun-def-h))

(define bsl-eval0 3)
(define bsl-eval1 (+ 1 1))
(define bsl-eval2 (* 3 10))
(define bsl-eval3 (+ (* 1 1) 10))

;;;; CONSTANTS ---------------------------------------------

(define NON-PARSABLE "error: non-parsable data given")
(define UNDEFINED-VARIABLE "error: undefined variable given")
(define UNDEFINED-FUNCTION "error: undefined function given")
(define NOT-BSL-FUN-EXPR "error: not BSL-fun-expr; cannot be evaluated")

(define CONST-DEF-CLOSE-TO-PI
  (make-const-def 'close-to-pi 3.14))
(define FUN-DEF-AREA-OF-CIRCLE
  (make-fun-def 'area-of-circle
                'r
                (make-mul 'close-to-pi (make-mul 'r 'r))))
(define FUN-DEF-VOLUME-OF-10-CYLINDER
  (make-fun-def 'volume-of-10-cylinder
                'r
                (make-mul 10 (make-fun-app 'area-of-circle 'r))))
(define CONST-DEF-X
  (make-const-def 'x 5))
(define CONST-DEF-Y
  (make-const-def 'y 3))
(define DEF-AREA
  (append
   (list CONST-DEF-CLOSE-TO-PI
         FUN-DEF-AREA-OF-CIRCLE
         FUN-DEF-VOLUME-OF-10-CYLINDER
         CONST-DEF-X
         CONST-DEF-Y)
   def-area-fgh)) ;; from DATA EXAMPLES

(define QUOTED-CONST-DEF-CLOSE-TO-PI
  '(define close-to-pi 3.14))
(define QUOTED-FUN-DEF-AREA-OF-CIRCLE
  '(define (area-of-circle r) (* close-to-pi (* r r))))
(define QUOTED-FUN-DEF-VOLUME-OF-10-CYLINDER
  '(define (volume-of-10-cylinder r) (* 10 (area-of-circle r))))
(define QUOTED-CONST-DEF-X
  '(define x 5))
(define QUOTED-CONST-DEF-Y
  '(define y 3))
(define QUOTED-FUN-DEF-F
  '(define (f x) (+ 3 x)))
(define QUOTED-FUN-DEF-G
  '(define (g y) (f (* 2 y))))
(define QUOTED-FUN-DEF-H
  '(define (h v) (+ (f v) (g v))))

(define L-QUOTED-DEF
  (list QUOTED-CONST-DEF-CLOSE-TO-PI
        QUOTED-FUN-DEF-AREA-OF-CIRCLE
        QUOTED-FUN-DEF-VOLUME-OF-10-CYLINDER
        QUOTED-CONST-DEF-X
        QUOTED-CONST-DEF-Y
        QUOTED-FUN-DEF-F
        QUOTED-FUN-DEF-G
        QUOTED-FUN-DEF-H))

;;;; FUNCTIONS ---------------------------------------------

;; S-expr SL -> BSL-fun-eval
;; evaluates a given sexpr according to given quoted definitions
(define (bsl-interpreter sexpr l-quoted-def)
  (bsl-evaluator (sexpr-parser sexpr)
                 (map quoted-def-parser l-quoted-def)))

(check-expect (bsl-interpreter sexpr0 L-QUOTED-DEF) 3)
(check-expect (bsl-interpreter sexpr1 L-QUOTED-DEF) 2)
(check-expect (bsl-interpreter sexpr2 L-QUOTED-DEF) 30)
(check-expect (bsl-interpreter sexpr3 L-QUOTED-DEF) 11)
(check-expect (bsl-interpreter sexpr4 L-QUOTED-DEF) 5)
(check-expect (bsl-interpreter sexpr5 L-QUOTED-DEF) 8)
(check-within (bsl-interpreter sexpr6 L-QUOTED-DEF) 7.5 0.001)
(check-expect (bsl-interpreter sexpr7 L-QUOTED-DEF) 34)
(check-expect (bsl-interpreter sexpr8 L-QUOTED-DEF) 5)
(check-expect (bsl-interpreter sexpr9 L-QUOTED-DEF) 25)
(check-expect (bsl-interpreter sexpr10 L-QUOTED-DEF) 65)
(check-expect (bsl-interpreter sexpr11 L-QUOTED-DEF) 225)
(check-within (bsl-interpreter '(area-of-circle 1) L-QUOTED-DEF) 3.14 0.001)
(check-within (bsl-interpreter '(volume-of-10-cylinder 1) L-QUOTED-DEF) 31.4 0.001)
(check-within (bsl-interpreter '(* 3 close-to-pi) L-QUOTED-DEF) 9.42 0.001)

(check-expect (bsl-interpreter sexpr0 '()) 3)
(check-error (bsl-interpreter sexpr4 '()) UNDEFINED-VARIABLE)
(check-error (bsl-interpreter '(undefined-function 2) L-QUOTED-DEF) UNDEFINED-FUNCTION)
(check-error (bsl-interpreter '(undefined-function undefined-variable) L-QUOTED-DEF) UNDEFINED-VARIABLE)
(check-error (bsl-interpreter "a string" L-QUOTED-DEF) NON-PARSABLE)
(check-error (bsl-interpreter '() L-QUOTED-DEF) NON-PARSABLE)
(check-error (bsl-interpreter '(1 2 3 4 5 6) L-QUOTED-DEF) NON-PARSABLE)

;; S-expr -> BSL-fun-expr
;; returns the BSL-fun-expr version of an S-expr
(define (sexpr-parser sexpr)
  (local (;; Atom -> BSL-fun-expr
          (define (parse-atom atom)
            (match atom
              [(or (? number?) (? symbol?)) atom]
              [_ (error NON-PARSABLE)]))
          ;; SL -> BSL-fun-expr
          (define (parse-sl sl)
            (match sl
              [(list '+ x y) (make-add (sexpr-parser x) (sexpr-parser y))]
              [(list '* x y) (make-mul (sexpr-parser x) (sexpr-parser y))]
              [(list (? symbol?) f-arg) (make-fun-app (car sl) (sexpr-parser f-arg))]
              [_ (error NON-PARSABLE)])))
    (match sexpr
      [(not (? pair?)) (parse-atom sexpr)]
      [_ (parse-sl sexpr)])))

(check-expect (sexpr-parser sexpr0) bsl-fun-expr0)
(check-expect (sexpr-parser sexpr1) bsl-fun-expr1)
(check-expect (sexpr-parser sexpr2) bsl-fun-expr2)
(check-expect (sexpr-parser sexpr3) bsl-fun-expr3)
(check-expect (sexpr-parser sexpr4) bsl-fun-expr4)
(check-expect (sexpr-parser sexpr5) bsl-fun-expr5)
(check-expect (sexpr-parser sexpr6) bsl-fun-expr6)
(check-expect (sexpr-parser sexpr7) bsl-fun-expr7)
(check-expect (sexpr-parser sexpr8) bsl-fun-expr8)
(check-expect (sexpr-parser sexpr9) bsl-fun-expr9)
(check-expect (sexpr-parser sexpr10) bsl-fun-expr10)
(check-expect (sexpr-parser sexpr11) bsl-fun-expr11)

(check-error (sexpr-parser "a string") NON-PARSABLE)
(check-error (sexpr-parser '()) NON-PARSABLE)
(check-error (sexpr-parser '(1 2 3 4 5 6 7)) NON-PARSABLE)

;; SL -> BSL-da
;; returns the BS-da-all version of an SL
(define (quoted-def-parser quoted-def)
  (match quoted-def
    [(list 'define (? symbol?) (? number?))
     (make-const-def (cadr quoted-def) (caddr quoted-def))]
    [(list 'define (list (? symbol?) (? symbol?)) f-body)
     (make-fun-def (caadr quoted-def) (cadadr quoted-def) (sexpr-parser f-body))]
    [_ (error NON-PARSABLE)]))

(check-within (quoted-def-parser QUOTED-CONST-DEF-CLOSE-TO-PI) CONST-DEF-CLOSE-TO-PI 0.001)
(check-expect (quoted-def-parser QUOTED-FUN-DEF-AREA-OF-CIRCLE) FUN-DEF-AREA-OF-CIRCLE)
(check-expect (quoted-def-parser QUOTED-FUN-DEF-VOLUME-OF-10-CYLINDER) FUN-DEF-VOLUME-OF-10-CYLINDER)
(check-expect (quoted-def-parser QUOTED-CONST-DEF-X) CONST-DEF-X)
(check-expect (quoted-def-parser QUOTED-CONST-DEF-Y) CONST-DEF-Y)
(check-expect (quoted-def-parser QUOTED-FUN-DEF-F) fun-def-f)
(check-expect (quoted-def-parser QUOTED-FUN-DEF-G) fun-def-g)
(check-expect (quoted-def-parser QUOTED-FUN-DEF-H) fun-def-h)
(check-within (map quoted-def-parser L-QUOTED-DEF) DEF-AREA 0.001)

(check-error (quoted-def-parser '()) NON-PARSABLE)
(check-error (quoted-def-parser "a string") NON-PARSABLE)
(check-error (quoted-def-parser '(1 2 3 4 5 6 7)) NON-PARSABLE)

;; BSL-fun-expr BSL-da-all -> BSL-eval
;; returns the BSL-eval of a bsl-fun-def
;; according to given def-area
(define (bsl-evaluator bsl-fun-expr def-area)
  (local (;; BSL-fun-expr -> BSL-eval
          (define (evaluate expr)
            (bsl-evaluator expr def-area)))
    (match bsl-fun-expr
      [(? number?) bsl-fun-expr]
      [(? symbol?)
       (const-def-value (lookup-const-def bsl-fun-expr def-area))]
      [(add x y) (+ (evaluate x) (evaluate y))]
      [(mul x y) (* (evaluate x) (evaluate y))]
      [(fun-app fun arg)
       (local ((define value (evaluate arg))
               (define f (lookup-fun-def fun def-area))
               (define substituted
                 (subst (fun-def-body f)
                        (fun-def-param f)
                        value)))
         (evaluate substituted))]
      [_ (error NOT-BSL-FUN-EXPR)])))

(check-expect (bsl-evaluator bsl-fun-expr0 DEF-AREA) 3)
(check-expect (bsl-evaluator bsl-fun-expr1 DEF-AREA) 2)
(check-expect (bsl-evaluator bsl-fun-expr2 DEF-AREA) 30)
(check-expect (bsl-evaluator bsl-fun-expr3 DEF-AREA) 11)
(check-expect (bsl-evaluator bsl-fun-expr4 DEF-AREA) 5)
(check-expect (bsl-evaluator bsl-fun-expr5 DEF-AREA) 8)
(check-within (bsl-evaluator bsl-fun-expr6 DEF-AREA) 7.5 0.001)
(check-expect (bsl-evaluator bsl-fun-expr7 DEF-AREA) 34)
(check-expect (bsl-evaluator bsl-fun-expr8 DEF-AREA) 5)
(check-expect (bsl-evaluator bsl-fun-expr9 DEF-AREA) 25)
(check-expect (bsl-evaluator bsl-fun-expr10 DEF-AREA) 65)
(check-expect (bsl-evaluator bsl-fun-expr11 DEF-AREA) 225)
(check-within (bsl-evaluator (make-fun-app 'area-of-circle 1) DEF-AREA) 3.14 0.001)
(check-within (bsl-evaluator (make-fun-app 'volume-of-10-cylinder 1) DEF-AREA) 31.4 0.001)
(check-within (bsl-evaluator (make-mul 3 'close-to-pi) DEF-AREA) 9.42 0.001)

(check-error (bsl-evaluator "a string" DEF-AREA) NOT-BSL-FUN-EXPR)
(check-error (bsl-evaluator '() DEF-AREA) NOT-BSL-FUN-EXPR)
(check-error (bsl-evaluator '(1 2 3 4 5 6) DEF-AREA) NOT-BSL-FUN-EXPR)
(check-error (bsl-evaluator sexpr11 DEF-AREA) NOT-BSL-FUN-EXPR)

;; Symbol BSL-def-all -> BSL-const-def
;; returns the BSL-const-def whose name matches const-name
(define (lookup-const-def const-name def-area)
  (local ((define lookup-const-result
            (filter (lambda (def)
                      (and (const-def? def)
                           (symbol=? (const-def-name def) const-name)))
                    def-area)))
    (if (null? lookup-const-result)
        (error UNDEFINED-VARIABLE)
        (car lookup-const-result))))

(check-expect (lookup-const-def 'close-to-pi DEF-AREA) CONST-DEF-CLOSE-TO-PI)
(check-expect (lookup-const-def 'x DEF-AREA) CONST-DEF-X)
(check-expect (lookup-const-def 'y DEF-AREA) CONST-DEF-Y)

(check-error (lookup-const-def 'z DEF-AREA) UNDEFINED-VARIABLE)
(check-error (lookup-const-def 'close-to-pi '()) UNDEFINED-VARIABLE)
(check-error (lookup-const-def 'x '()) UNDEFINED-VARIABLE)
(check-error (lookup-const-def 'y '()) UNDEFINED-VARIABLE)

;; Symbol BSL-def-all -> BSL-fun-def
;; ;; returns the BSL-fun-def whose name matches f-name
(define (lookup-fun-def f-name def-area)
  (local ((define lookup-fun-result
            (filter (lambda (def)
                      (and (fun-def? def)
                           (symbol=? (fun-def-name def) f-name)))
                    def-area)))
    (if (null? lookup-fun-result)
        (error UNDEFINED-FUNCTION)
        (car lookup-fun-result))))

(check-expect (lookup-fun-def 'area-of-circle DEF-AREA) FUN-DEF-AREA-OF-CIRCLE)
(check-expect (lookup-fun-def 'volume-of-10-cylinder DEF-AREA) FUN-DEF-VOLUME-OF-10-CYLINDER)
(check-expect (lookup-fun-def 'f DEF-AREA) fun-def-f)
(check-expect (lookup-fun-def 'g DEF-AREA) fun-def-g)
(check-expect (lookup-fun-def 'h DEF-AREA) fun-def-h)

(check-error (lookup-fun-def 'i DEF-AREA) UNDEFINED-FUNCTION)
(check-error (lookup-fun-def 'area-of-circle '()) UNDEFINED-FUNCTION)
(check-error (lookup-fun-def 'volume-of-10-cylinder '()) UNDEFINED-FUNCTION)
(check-error (lookup-fun-def 'f '()) UNDEFINED-FUNCTION)
(check-error (lookup-fun-def 'g '()) UNDEFINED-FUNCTION)
(check-error (lookup-fun-def 'h '()) UNDEFINED-FUNCTION)

;; BSL-fun-expr Symbol Number -> BSL-expr
;; replaces all instances of param in a bsl-fun-expr with arg,
;; thus producing a BSL-expr
(define (subst bsl-fun-expr param arg)
  (match bsl-fun-expr
    [(? number?) bsl-fun-expr]
    [(? symbol?) (if (symbol=? bsl-fun-expr param) arg bsl-fun-expr)]
    [(add x y) (make-add (subst x param arg) (subst y param arg))]
    [(mul x y) (make-mul (subst x param arg) (subst y param arg))]
    [(fun-app f-name f-arg) (make-fun-app f-name (subst f-arg param arg))]
    [_ (error NOT-BSL-FUN-EXPR)]))

(check-expect (subst bsl-fun-expr4 'x 5) 5)
(check-expect (subst bsl-fun-expr5 'x 5) (make-add 5 3))
(check-expect (subst bsl-fun-expr6 'x 5) (make-mul 1/2 (make-mul 5 3)))
(check-expect (subst bsl-fun-expr7 'x 5) (make-add (make-mul 5 5) (make-mul 'y 'y)))
(check-expect (subst bsl-fun-expr11 'x 5) (make-mul (make-fun-app 'g (make-fun-app 'h 5)) bsl-fun-expr8))

(check-error (subst (make-fun-def 'f 'param (symbol->string 'body)) 'x 5) NOT-BSL-FUN-EXPR)

(test)
