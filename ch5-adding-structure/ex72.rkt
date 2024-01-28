#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define-struct centry [name home office cell])
;; a C-Entry is a structure:
;;  (make-centry String Phone Phone Phone)
;; interpretation: a contact's name and phone numbers

(define-struct phone [area-code num])
;; a Phone is a structure:
;;  (make-phone Number String)
;; interpretation: a phone number and its applicable area code

(define-struct phone# [area switch num])
;; a Phone# is a structure:
;;  (make-phone# Number Number Number)
;; interpretation: a collection of digits that make up a US phone
;; number; starting from the left, digits at positions:
;; - [0,2] indicate the area code
;; - [3,5] indicate the code for the phone switch
;; - [6,9] indicate the phone with respect to the neighborhood
