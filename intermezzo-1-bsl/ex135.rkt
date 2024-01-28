#lang racket

;; 1.
(define-struct oops [])
;; oops is a variable
;; legal because satisfies the form (define-struct variable [variable variable ...])
;; a structure does not need to have fields

;; 2.
(define-struct child [parents dob date])
;; child is a variable
;; parents, dob, and date are variables enclosed in parentheses
;; satisfies the form (define-struct variable [variable variable ...])
;; legal

;; 3.
(define-struct (child person) [dob date])
;; (child person) is a function application
;; does not satisfy the form (define-struct variable [variable variable ...])
;; illegal
