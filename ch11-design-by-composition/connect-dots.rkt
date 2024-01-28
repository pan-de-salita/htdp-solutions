#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require lang/posn)

;;; data definitions

;; an NE-LoP is one of:
;; - (cons Posn '())
;; - (cons Posn NE-LoP)
;; i.e. a non-empty List-of-Posns
;; examples:
(define triangle-p
  (list (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 30 20)))
(define square-p
  (list (make-posn 10 10)
        (make-posn 20 10)
        (make-posn 20 20)
        (make-posn 10 20)))

;;; constants

(define CANVAS (empty-scene 50 50 "dimgrey"))
(define POLYGON-COLOR "green")

;;; functions

;; Image NE-LoP -> Image
;; connects the dots in an NE-LoP p by rendering
;; lines in Image img

(check-expect (connect-dots CANVAS triangle-p)
              (scene+line
               (scene+line CANVAS 20 20 30 20 POLYGON-COLOR)
               20 10 20 20 POLYGON-COLOR))

(define (connect-dots img p)
  img)



;;; application

(test)
