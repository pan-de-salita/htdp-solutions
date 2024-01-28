#lang racket
(require test-engine/racket-tests)

;; a [List X Y] is a list:
;;   (cons X (cons Y '()))

;; a [List Number Number] is a list:
;;   (cons Number (cons Number '()))
(define pair/num (cons 0 (cons 1 '())))

;; a [List Number 1String] is a list:
;;   (cons Number (cons 1String '()))
(define pair/num-1string (cons 0 (cons "a" '())))

;; a [List String Boolean] is a list:
;;   (cons String (cons Boolean '()))
(define pair/str-bool (cons "string" (cons #f '())))
