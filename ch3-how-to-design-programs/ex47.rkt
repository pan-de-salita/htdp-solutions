;; Differs from exercise requirements. The program allows for the happiness
;; level to drop below 0. In cases where the happiness level drops below 0,
;; a measure is in place to have it rise back up.

#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require test-engine/racket-tests)

;; Constant definitions
;; All capitalized instances of "H" mean "happiness".
(define BACKGROUND-WIDTH 320)
(define BACKGROUND-HEIGHT 120)

(define H-PER-TICK -0.1)
(define H-PER-DOWN -1/5)
(define H-PER-UP 1/3)

(define H-GAUGE-WIDTH (* BACKGROUND-WIDTH 0.8))
(define H-GAUGE-HEIGHT (* BACKGROUND-HEIGHT 0.15))

(define H-DISPLAY-WIDTH H-GAUGE-WIDTH)
(define H-DISPLAY-HEIGHT H-GAUGE-HEIGHT)
(define H-DISPLAY-X-COR (/ (- BACKGROUND-WIDTH H-GAUGE-WIDTH) 2))
(define H-DISPLAY-Y-COR (/ BACKGROUND-HEIGHT 2))

(define H-MAX H-DISPLAY-WIDTH)
(define H-MIN 0)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT "darkgray"))
(define H-GAUGE (rectangle H-GAUGE-WIDTH H-GAUGE-HEIGHT "outline" "black"))
(define WORLD (overlay H-GAUGE BACKGROUND))

;; A WorldState is a Number.
;; H-DISPLAY lengthens or shortens according to WorldState.

;; WorldState -> Number
;; Computes the appropriate width of H-DISPLAY.
(define (h-display-width ws)
  (cond [(<= ws H-MIN) H-MIN]
        [(> ws H-MAX) H-MAX]
        [else ws]))

(check-within (h-display-width 0) 0 0.01)
(check-within (h-display-width 1000) H-MAX 0.01)
(check-within (h-display-width 40) 40 0.01)

;; WorldState -> Image
;; Displays H-DISPLAY according to WorldState.
(define (h-display ws)
  (rectangle (h-display-width ws)
             H-DISPLAY-HEIGHT
             "solid"
             "red"))

(check-expect (h-display -1) (rectangle 0 H-GAUGE-HEIGHT "solid" "red"))
(check-expect (h-display 500) (rectangle H-MAX H-GAUGE-HEIGHT "solid" "red"))
(check-expect (h-display 50) (rectangle 50 H-GAUGE-HEIGHT "solid" "red"))

;; WorldState -> Image
;; Renders the H-GAUGE according to the WorldState provided.
(define (render ws)
  (place-image/align (h-display ws)
                     H-DISPLAY-X-COR
                     H-DISPLAY-Y-COR
                     "left"
                     "middle"
                     WORLD))

(check-expect (render 0)
              (place-image/align (h-display 0)
                                 H-DISPLAY-X-COR H-DISPLAY-Y-COR
                                 "left" "middle"
                                 WORLD))
(check-expect (render 50)
              (place-image/align (h-display 50)
                                 H-DISPLAY-X-COR H-DISPLAY-Y-COR
                                 "left" "middle"
                                 WORLD))

;; WorldState -> WorldState
;; Makes H-DISPLAY decrease in length by H-PER-TICK per clock tick.
(define (clock-tick-handler ws)
  (+ ws H-PER-TICK))

(check-within (clock-tick-handler 0) -0.1 0.01)
(check-within (clock-tick-handler 50) 49.9 0.01)

;; WorldState String -> WorldState
;; Makes H-DISPLAY decrease/increase in length according to keystroke.
(define (keystroke-handler ws ke)
  (cond [(string=? ke "down") (+ ws (* ws H-PER-DOWN))]
        [(string=? ke "up") (if (< ws 0)
                                (abs (+ ws (* ws H-PER-UP)))
                                (+ ws (* ws H-PER-UP)))]
        [else ws]))

(check-within (keystroke-handler 0 "up") (+ 0 (* 0 H-PER-UP)) 0.01)
(check-within (keystroke-handler 50 "down") (+ 50 (* 50 H-PER-DOWN)) 0.01)
(check-within (keystroke-handler 20 "f") 20 0.01)

;; WorldState -> WorldState
;; Runs the program from some initial state.
(define (gauge-prog ws)
  (big-bang ws
    [to-draw render]
    [on-tick clock-tick-handler]
    [on-key keystroke-handler]))

(test)
(gauge-prog H-MAX)
