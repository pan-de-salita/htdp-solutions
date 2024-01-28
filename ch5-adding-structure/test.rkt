#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;; (define-struct centry [name home office cell])
;; (define-struct phone [area-code num])

;; (define contact-list-1
;;   (make-centry "Shiriam Fisler"
;;                (make-phone 123 "1234567")
;;                (make-phone 123 "1234567")
;;                (make-phone 123 "1234567")))

;;;;;;;;;;

(define-struct entry [name phone email])
;; an Entry is a structure:
;;  (make-entry String String String)
;; interpretation: a contact's name, phone#, and email

(define pl (make-entry "Al Abe" "666-7771" "lee@x.me"))
(make-entry "Tara Harp" "666-7770" "th@smlu.edu")

(define-struct ball1d [location velocity])
;; a Ball-1d is a structure:
;;  (make-ball Number Number)
;; interpretation 1: distance to top and velocity
;; interpretation 2: distance to left and velocity

(define-struct ball2d [locaton velocity])
;; a Ball-2d is a structure:
;;  (make-ball2d Posn Vel)
;; interpretation: a 2-dimensional position and velocity

(define-struct vel [deltax deltay])
;; a Vel is a structure:
;;  (make-vel Number Number)
;; interpretation (make-vel dx dy) means velocity of:
;; dx pixels [per tick] along the horizontal and
;; dy pixels [per tick] along the vertical direction

(define ball2d-model
  (make-ball2d (make-posn 30 40)
             (make-vel -10 5)))

(define-struct ballf [x y deltax deltay])

;;;;;;;;;;
