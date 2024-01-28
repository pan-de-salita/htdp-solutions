#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require lang/posn)

;;; data definitions

;; a Polygon is one of:
;; - (list Posn Posn Posn)
;;   OR (cons Posn (cons Posn (cons Posn '())))
;; - (cons Posn Polygon)
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

;; an NE-LoP is one of:
;; - (cons Posn '())
;; - (cons Posn NE-LoP)
;; i.e. a non-empty List-of-Posns

;;; constants

(define CANVAS (empty-scene 50 50 "dimgrey"))
(define POLYGON-COLOR "green")

;;; functions

;; Image Polygon -> Image
;; adds an image of Polygon p to Image img

(check-expect (render-poly-v4 CANVAS triangle-p)
              (scene+line
               (scene+line
                (scene+line CANVAS 30 20 20 10 POLYGON-COLOR)
                20 20 30 20 POLYGON-COLOR)
               20 10 20 20 POLYGON-COLOR))
(check-expect (render-poly-v4 CANVAS square-p)
              (scene+line
               (scene+line
                (scene+line
                 (scene+line CANVAS 10 20 10 10 POLYGON-COLOR)
                 20 20 10 20 POLYGON-COLOR)
                20 10 20 20 POLYGON-COLOR)
               10 10 20 10 POLYGON-COLOR))

(define (render-poly-v4 img p)
  (connect-dots-v2 img p (first p)))

;; Image NE-LoP Posn -> Image
;; version 2 of connect-dots; consumes an additional Posn
;; to w/c the last Posn is connected

(check-expect (connect-dots-v2 CANVAS triangle-p (first triangle-p))
              (scene+line
               (scene+line
                (scene+line CANVAS 30 20 20 10 POLYGON-COLOR)
                20 20 30 20 POLYGON-COLOR)
               20 10 20 20 POLYGON-COLOR))
(check-expect (connect-dots-v2 CANVAS square-p (first square-p))
              (scene+line
               (scene+line
                (scene+line
                 (scene+line CANVAS 10 20 10 10 POLYGON-COLOR)
                 20 20 10 20 POLYGON-COLOR)
                20 10 20 20 POLYGON-COLOR)
               10 10 20 10 POLYGON-COLOR))

(define (connect-dots-v2 img p posn)
  (cond [(empty? (rest p)) (render-line img (first p) posn)]
        [else (render-line (connect-dots-v2 img (rest p) posn)
                           (first p) (second p))]))

;; Image Posn Posn -> Image
;; draws a red line from Posn p to Posn q into image img

(check-expect (render-line CANVAS (make-posn 10 10) (make-posn 20 10))
              (scene+line CANVAS 10 10 20 10 POLYGON-COLOR))

(define (render-line img p q)
  (scene+line img
              (posn-x p) (posn-y p)
              (posn-x q) (posn-y q)
              POLYGON-COLOR))

;;; application

(test)
