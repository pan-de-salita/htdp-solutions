#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define-struct movie [title producer year])
;; a Movie is a structure:
;;  (make-movie String String Year)

;; a Year is a four-digit Number, e.g., 2001
;; interpretation: year a movie was released

(define-struct person [name hair eyes phone])
;; a Person is a structure:
;;  (make-person String String String Phone-Number)

;; a Phone-Number is a String of Numbers with
;; the first three Numbers being followed by
;; a hyphen, e.g., "123-4567"
;; interpretation: American phone number

(define-struct pet [name number])
;; a Pet is a structure:
;;  (make-pet String Pet-Number)

;; a Pet Number is a Number
;; interpretation: the order a pet was brought home
;; in relation to other pets owned

(define-struct CD [artist title price])
;; a CD is a structure:
;;  (make-CD String String Number)

(define-struct sweater [material size producer])
;; a Sweater is a structure:
;;  (make-sweater String Size String)

;; a Size is one of the following Strings:
;; - "XS"
;; - "S"
;; - "M"
;; - "L"
;; - "XL"
;; interpretation: sweater size
