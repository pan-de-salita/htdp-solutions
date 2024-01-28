#lang racket
(require test-engine/racket-tests)

;; an Xexpr.v0 (short for X-expression) is a one-item list:
;;   (cons Symbol '())
(define xexpr.v0-0 '(machine))

;; an Xexpr.v1 is a list:
;;   (cons Symbol [List-of Xexpr.v1])
(define xexpr.v1-0 '(machine (action)))
(define xexpr.v1-1 '(machine (action) (action) (action)))

;; an Xexpr.v2 is a cons that begins with a Symbol
;; followed by one of:
;;   - Body
;;   - (cons Attribute Body)
;; where Body is short for either:
;;   - '()
;;   - (cons (list Xexpr.v2) Body)
;; an Attribute is one of:
;;   - '()
;;   - (cons (list Symbol String) Attribute)
;; NOTE: old definition
(define xexpr.v2-0 xexpr.v0-0) ;; '(machine)
;; shape:
;; (cons Symbol '())
;; == (cons Symbol Body)
(define xexpr.v2-1 xexpr.v1-0) ;; '(machine (action))
;; shape:
;; (cons Symbol (cons (list Xexpr.v2) '()))
;; == (cons Symbol Body)
(define xexpr '(machine (action) (action)))
;; shape:
;; (cons Symbol (cons (list Xexpr.v2) (cons (list Xexpr.v2) (cons (list Xexpr.v2) '()))))
;; == (cons Symbol Body)
(define xexpr.v2-2 '(machine ((initial "red"))))
;; shape:
;; (cons Symbol (cons (list Symbol String) '()))
;; == (cons Symbol (cons Attribute Body))
(define xexpr.v2-3
  '(machine ((initial "red"))
            (action ((state "red") (next "green")))
            (action ((state "green") (next "yellow")))
            (action ((state "yellow") (next "red")))))
;; shape:
;; (list Symbol (list (list Symbol String))
;;       (list Symbol (list (list Symbol String) (list Symbol String)))
;;       (list Symbol (list (list Symbol String) (list Symbol String)))
;;       (list Symbol (list (list Symbol String) (list Symbol String))))
;; == (cons Symbol (cons Attribute Body))
