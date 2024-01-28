#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define EMPTY-SCENE (empty-scene 100 100 "black"))

(define DOT-RADIUS 4)
(define DOT (circle DOT-RADIUS "solid" "red"))
(define DOT-SPEED 2)
(define DOT-INIT-X-COORDINATE (- 0 (image-width DOT)))
(define DOT-INIT-Y-COORDINATE (/ (image-height EMPTY-SCENE) 2))
(define DOT-INIT-POSITION (make-posn DOT-INIT-X-COORDINATE DOT-INIT-Y-COORDINATE))

(define EMPTY-SCENE-LIMIT (+ 100 (image-width DOT)))

;; a Posn represents the state of the world
;;  (make-posn Number Number)
;; interpretation: a point x pixels from left, y from top

;; Posn -> Image
;; renders the image of DOT on EMPTY-SCENE according to given Posn
(check-expect (scene+dot (make-posn 33 44))
              (place-image DOT
                           (posn-x (make-posn 33 44))
                           (posn-y (make-posn 33 44))
                           EMPTY-SCENE))
(check-expect (scene+dot (make-posn 4 80))
              (place-image DOT
                           (posn-x (make-posn 4 80))
                           (posn-y (make-posn 4 80))
                           EMPTY-SCENE))

(define (scene+dot position)
  (place-image DOT
               (posn-x position)
               (posn-y position)
               EMPTY-SCENE))

;; Posn -> Posn
;; moves DOT by 3 pixels to right; DOT reappears at DOT-INIT-COORDINATES
;; when out of frame
(check-expect (x+ (make-posn 33 44))
              (make-posn (+ 33 DOT-SPEED) 44))
(check-expect (x+ (make-posn EMPTY-SCENE-LIMIT 23))
              (make-posn DOT-INIT-X-COORDINATE 23))

(define (x+ position)
  (if (not (>= (posn-x position) EMPTY-SCENE-LIMIT))
      (make-posn (+ (posn-x position) DOT-SPEED) (posn-y position))
      (make-posn DOT-INIT-X-COORDINATE (posn-y position))))

;; Posn Number Number MouseEvent -> Posn
;; DOT appears at coordinate of user's last mouse click; otherwise
;; position remains unchanged
(check-expect (reset-dot (make-posn 33 44) 56 45 "button-up")
              (make-posn 56 45))
(check-expect (reset-dot (make-posn 33 44) 56 45 "button-down")
              (make-posn 33 44))

(define (reset-dot position mouse-x mouse-y mouse-event)
  (if (mouse=? "button-up" mouse-event)
      (make-posn mouse-x mouse-y)
      position))

;; Posn -> Posn
(define (main p0)
  (big-bang p0
            [on-tick x+]
            [on-mouse reset-dot]
            [to-draw scene+dot]))

(test)
(main DOT-INIT-POSITION)
