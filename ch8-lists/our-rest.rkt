#lang racket

(define-struct pair [left right])
;; a ConsOrEmpty is one of:
;; - '()
;; - (make-pair Any ConsOrEmpty)
;; i.e. the class of all lists.

;; Any Any -> ConsOrEmpty
(define (our-cons a-value a-list)
  (cond
    [(empty? a-list) (make-pair a-value a-list)]
    [(pair? a-list) (make-pair a-value a-list)]
    [else (error "cons: second argument must be a list")]))

;; ConsOrEmpty -> Any
;; i.e. extracts the left part of the given pair
(define (our-first a-list)
  (if (empty? a-list)
      (error "our-first: list cannot be empty")
      (pair-left a-list)))

;; ConsOrEmpty -> Any
;; i.e. extracts the right part of the given pair
(define (our-rest a-list)
  (if (empty? a-list)
      (error "our-rest: list cannot be empty")
      (pair-right a-list)))
