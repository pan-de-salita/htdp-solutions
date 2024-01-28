#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

(define BACKGROUND (empty-scene 300 100 "black"))
(define MSG-X-COORDINATE (/ (image-width BACKGROUND) 2))
(define MSG-Y-COORDINATE (/ (image-height BACKGROUND) 2))
(define MSG-FONT-SIZE 14)
(define MSG-FONT-COLOR "white")

(define-values
  (LOCKED
   CLOSED
   OPEN)
  (values "l o c k e d"
          "c l o s e d"
          "o p e n"))

;; a DoorState is one of:
;; - LOCKED
;; - CLOSED
;; - OPEN

(define-values
  (UNLOCK
   PUSH
   LOCK)
  (values "u"
          " "
          "l"))

;; DoorState -> image
;; renders a message communicating the given DoorState
(check-expect (door-render LOCKED)
              (place-image (text (string-append "d o o r   " LOCKED)
                                 MSG-FONT-SIZE MSG-FONT-COLOR)
                           MSG-X-COORDINATE
                           MSG-Y-COORDINATE
                           BACKGROUND))
(check-expect (door-render CLOSED)
              (place-image (text (string-append "d o o r   " CLOSED)
                                 MSG-FONT-SIZE MSG-FONT-COLOR)
                           MSG-X-COORDINATE
                           MSG-Y-COORDINATE
                           BACKGROUND))
(check-expect (door-render OPEN)
              (place-image (text (string-append "d o o r   " OPEN)
                                 MSG-FONT-SIZE MSG-FONT-COLOR)
                           MSG-X-COORDINATE
                           MSG-Y-COORDINATE
                           BACKGROUND))

(define (door-render doorstate)
  (place-image (text (string-append "d o o r   " doorstate)
                     MSG-FONT-SIZE MSG-FONT-COLOR)
               MSG-X-COORDINATE
               MSG-Y-COORDINATE
               BACKGROUND))

;; DoorState -> DoorState
;; changes DoorState when user presses appropriate key
(check-expect (doorstate-key-controller LOCKED "x") LOCKED)
(check-expect (doorstate-key-controller LOCKED UNLOCK) CLOSED)
(check-expect (doorstate-key-controller CLOSED "y") CLOSED)
(check-expect (doorstate-key-controller CLOSED PUSH) OPEN)
(check-expect (doorstate-key-controller CLOSED LOCK) LOCKED)
(check-expect (doorstate-key-controller OPEN "z") OPEN)
(check-expect (doorstate-key-controller OPEN "?") OPEN)

(define (doorstate-key-controller doorstate key)
  (cond ((and (string=? key UNLOCK)
              (string=? doorstate LOCKED))
         CLOSED)
        ((and (string=? key PUSH)
              (string=? doorstate CLOSED))
         OPEN)
        ((and (string=? key LOCK)
              (string=? doorstate CLOSED))
         LOCKED)
        (else doorstate)))

;; DoorState -> DoorState
;; changes DoorState from OPEN over the period of one tick
(check-expect (door-closer LOCKED) LOCKED)
(check-expect (door-closer CLOSED) CLOSED)
(check-expect (door-closer OPEN) CLOSED)

(define (door-closer doorstate)
  (cond ((string=? doorstate LOCKED) LOCKED)
        ((string=? doorstate CLOSED) CLOSED)
        ((string=? doorstate OPEN) CLOSED)))

;; DoorState -> DoorState
(define (door-simulation doorstate)
  (big-bang doorstate
            [to-draw door-render]
            [on-key doorstate-key-controller]
            [on-tick door-closer 3]))

(test)
(door-simulation LOCKED)
