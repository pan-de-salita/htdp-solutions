#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;; a Color is one of:
;; - "white"
;; - "yellow"
;; - "orange"
;; - "green"
;; - "red"
;; - "blue"
;; - "black"

;; examples:
(square 10 "solid" "red")
(circle 5 "solid" "black")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; H is a Number between 0 and 100
;; interpretation: represents a happiness value

;; examples:
0 ;; least happy
100 ;; happiest
50 ;; in between least happy and happiest

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct person [fstname lstname male?])
;; a Person is a structure:
;;   (make-person String String Boolean)

;; examples:
(make-person "Gilles" "Deleuze" #t)
(make-person "Sadie" "Plant" #f)

;; note: probably best not to use a field name that
;; looks like a predicate because entries can lack
;; expressivity. compare the following, for example:

(make-person "Felix" "Guattari" #t)
(make-person "Felix" "Guattari" "male") ;; indicates the person's gender more clearly

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct dog [owner name age happiness])
;; a Dog is a structure:
;;  (make-dog Person String PositiveInteger H)

;; examples:
(make-dog (make-person
           "Gilles" "Deleuze" #t)
          "Alain" 5 0)
(make-dog (make-person
           "Jaques" "Derrida" #t)
          "Jordan" 3 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a Weapon is one of:
;; - #f
;; - Posn
;; interpretation: #f means the missile hasn't
;; been fired yet; a Posn means it is in flight

;; examples:
#f ;; missile hasn't been fired yet
(make-posn 10 10) ;; missile is in flight and is located at (10,10)
(make-posn 0 0) ;; missile is in flight and is located at (0,0)
