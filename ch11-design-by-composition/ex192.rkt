#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require lang/posn)

;;; data definitions

;; a Polygon is one of:
;; - (list Posn Posn Posn) OR (cons Posn (cons Posn (cons Posn '())))
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

(check-expect (render-poly CANVAS triangle-p)
              (render-line (connect-dots CANVAS triangle-p)
                           (first triangle-p) (last triangle-p)))
(check-expect (render-poly CANVAS square-p)
              (render-line (connect-dots CANVAS square-p)
                           (first square-p) (last square-p)))

(define (render-poly img p)
  (render-line (connect-dots img p) (first p) (last p)))

;; Image NE-LoP -> Image
;; connects the dots in an NE-LoP p by rendering
;; lines in Image img

(check-expect (connect-dots CANVAS triangle-p)
              (scene+line
               (scene+line CANVAS 20 20 30 20 POLYGON-COLOR)
               20 10 20 20 POLYGON-COLOR))
(check-expect (connect-dots CANVAS square-p)
              (scene+line
               (scene+line
                (scene+line CANVAS 20 20 10 20 POLYGON-COLOR)
                20 10 20 20 POLYGON-COLOR)
               10 10 20 10 POLYGON-COLOR))

(define (connect-dots img p)
  (cond [(empty? (rest p)) img]
        [else (render-line (connect-dots img (rest p)) (first p) (second p))]))

;; Image Posn Posn -> Image
;; draws a red line from Posn p to Posn q into image img

(check-expect (render-line CANVAS (make-posn 10 10) (make-posn 20 10))
              (scene+line CANVAS 10 10 20 10 POLYGON-COLOR))

(define (render-line img p q)
  (scene+line img
              (posn-x p) (posn-y p)
              (posn-x q) (posn-y q)
              POLYGON-COLOR))

;; Polygon -> Posn
;; extracts the last item from a NE-LoP l

(check-expect (last triangle-p) (make-posn 30 20))
(check-expect (last square-p) (make-posn 10 20))

(define (last-p p)
  (cond [(empty? (rest (rest (rest p)))) (third p)]
        [else (last-p (rest p))]))

;; it's acceptable to use last on Polygons because Polygons by
;; definition contain at least three Posns, making them instances
;; of NE-LOPs.
;;
;; using the template for connect-dots to design last is also
;; appropriate because Polygons are instances of NE-LoPs:

;; NE-Lop -> Posn
;; extracts the last item from a NE-LoP l

(check-expect (last-l triangle-p) (last triangle-p))
(check-expect (last-l square-p) (last square-p))
(check-expect (last-l (list (make-posn 0 0))) (make-posn 0 0)) ;; works for last-l, but not for last

(define (last-l l)
  (cond [(empty? (rest l)) (first l)]
        [else (last-l (rest l))]))

;;; application

(test)
