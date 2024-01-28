#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require lang/posn)

;; a Polygon is one of:
;; - (list Posn Posn Posn)
;; - (cons Posn Polygon)
(define triangle-p
  (list
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 30 20)))
 (define square-p
  (list
    (make-posn 10 10)
    (make-posn 20 10)
    (make-posn 20 20)
    (make-posn 10 20)))

(define MT (empty-scene 50 50))

;; Image Polygon -> Image
;; adds a corner of p to img

(check-expect (render-poly MT triangle-p)
              (scene+line
               (scene+line
                (scene+line MT 20 10 20 20 "red")
                20 20 30 20 "red")
               30 20 20 10 "red"))
(check-expect (render-poly MT square-p)
              (scene+line
               (scene+line
                (scene+line
                 (scene+line MT 10 10 20 10 "red")
                 20 10 20 20 "red")
                20 20 10 20 "red")
               10 20 10 10 "red"))

(define (render-poly img p)
  (render-line (connect-dots img p) (car p) (last p)))

;; Image NELop -> Image
;; connects the Posns in p in an image

(define (connect-dots img p)
  (cond [(empty? (cdr p)) img]
        [else (render-line (connect-dots img (cdr p))
                           (car p)
                           (cadr p))]))

;; Image Posn Posn -> Image
;; draws a red line from Posn p to Posn q into im

(define (render-line im p q)
  (scene+line im
              (posn-x p) (posn-y p)
              (posn-x q) (posn-y q)
              "red"))

;; Polygon -> Posn
;; extracts the last item from p

(define (last p)
  (cond [(empty? (cdddr p)) (caddr p)]
        [else (last (cdr p))]))

;;;; render-poly function with local definitions

;; Image Polygon -> Image
;; render-poly-local works just like render-poly

(check-expect (render-poly-local MT triangle-p) (render-poly MT triangle-p))
(check-expect (render-poly-local MT square-p) (render-poly MT square-p))

(define (render-poly-local img p)
  (local (;; Image Posn Posn -> Image
          ;; draws a red line from Posn p to Posn q into im
          (define (render-line im p q)
            (scene+line im
                        (posn-x p) (posn-y p)
                        (posn-x q) (posn-y q)
                        "red"))
          ;; Image NELop -> Image
          ;; connects the Posns in p in an image
          (define (connect-dots img p)
            (cond [(empty? (cdr p)) img]
                  [else (render-line (connect-dots img (cdr p))
                                     (car p)
                                     (cadr p))])))
    (render-line (connect-dots img p) (car p) (own-last p))))

;; Polygon -> Posn
;; extracts the last item from p
(define (own-last p)
  (cond [(empty? (cdddr p)) (caddr p)]
        [else (own-last (cdr p))]))

(test)
