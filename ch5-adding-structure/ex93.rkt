#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;; constants and data definitions

(define CHAM (bitmap "./chameleon.png"))
(define CHAM-WIDTH (image-width CHAM))
(define CHAM-HEIGHT (image-height CHAM))

(define CANVAS-WIDTH (* 3 CHAM-WIDTH))
(define CANVAS-HEIGHT (* 1.5 CHAM-WIDTH))

(define CHAM-X-START (-(/ CHAM-WIDTH 2)))
(define CHAM-Y (/ CANVAS-HEIGHT 2))
(define CHAM-RESET (+ CANVAS-WIDTH (/ CHAM-WIDTH 2)))
(define CHAM-VELOCITY 3)

(define JOY/TICK 0.65)
(define JOY/DOWN 2)
(define JOY-GAUGE-WIDTH (* CANVAS-WIDTH 0.5))
(define JOY-GAUGE-HEIGHT (* CANVAS-HEIGHT 0.08))

(define JOY-LEVEL-COLOR "black")
(define JOY-LEVEL-HEIGHT JOY-GAUGE-HEIGHT)
(define JOY-LEVEL-X (/ (- CANVAS-WIDTH JOY-GAUGE-WIDTH) 2))
(define JOY-LEVEL-Y (- CANVAS-HEIGHT (/ JOY-GAUGE-HEIGHT 2)))
(define JOY-MAX JOY-GAUGE-WIDTH)
(define JOY-MIN 0)

(define INSTRUCTION (text "press ↓ please, thanks" 18 "white"))
(define TEXTBOX (overlay INSTRUCTION
                          (rectangle (image-width INSTRUCTION)
                                     (image-height INSTRUCTION)
                                     "solid" "black")))
(define JOY-GAUGE (rectangle JOY-GAUGE-WIDTH JOY-GAUGE-HEIGHT "outline" "black"))
(define JOY-GAUGE-POSN (make-posn (/ CANVAS-WIDTH 2)
                                  (- CANVAS-HEIGHT
                                     (+ (image-height INSTRUCTION)
                                        (/ (image-height INSTRUCTION) 2)))))
(define BACKGROUND
  (beside (empty-scene (* 1/3 CANVAS-WIDTH) CANVAS-HEIGHT "red")
          (empty-scene (* 1/3 CANVAS-WIDTH) CANVAS-HEIGHT "white")
          (empty-scene (* 1/3 CANVAS-WIDTH) CANVAS-HEIGHT "blue")))
(define CANVAS (place-image TEXTBOX
                            (/ CANVAS-WIDTH 2)
                            (+ (image-height INSTRUCTION)
                               (/ (image-height INSTRUCTION) 2))
                            BACKGROUND))

;; an XCor is a Number.
;; represents the x-coordinate of the chamelion as it walks
;; across canvas.

;; a Joy is a Number.
;; represents the joy level of the chameleon.

;; a ChamColor is one of the following:
;; - "red"
;; - "green"
;; - "blue"
;; represetnts the color of the chameleon.

(define-struct VCham [x-cor joy color])
;; VCham is a structure:
;;     (make-VCham XCor Joy ChamColor)
;; (make-VCham xc h cc) describes chameleon's
;; - x-coordinate
;; - happiness
;; - color
(define VCham-ex0 (make-VCham CHAM-X-START JOY-MAX "red"))
(define VCham-ex1 (make-VCham (* 1/3 CANVAS-WIDTH) (* 2/3 JOY-GAUGE-WIDTH) "red"))
(define VCham-ex2 (make-VCham (* 1/2 CANVAS-WIDTH) (* 1/2 JOY-GAUGE-WIDTH) "green"))
(define VCham-ex3 (make-VCham (* 2/3 CANVAS-WIDTH) (* 1/3 JOY-GAUGE-WIDTH) "blue"))
(define VCham-ex4 (make-VCham CHAM-RESET JOY-MIN "red"))

;;; functions

;; VCham -> Image
;; places the image of chameleon and joy gauge on canvas.
(define (cham-walking vch)
  (place-images
   (list (draw-cham-color (VCham-color vch))
         (draw-joy-level (VCham-joy vch)))
   (list (make-posn (VCham-x-cor vch) CHAM-Y)
         JOY-GAUGE-POSN)
   CANVAS))

(check-expect (cham-walking VCham-ex0)
              (place-images
               (list (draw-cham-color (VCham-color VCham-ex0))
                     (draw-joy-level (VCham-joy VCham-ex0)))
               (list (make-posn (VCham-x-cor VCham-ex0) CHAM-Y)
                     JOY-GAUGE-POSN)
               CANVAS))
(check-expect (cham-walking VCham-ex1)
              (place-images
               (list (draw-cham-color (VCham-color VCham-ex1))
                     (draw-joy-level (VCham-joy VCham-ex1)))
               (list (make-posn (VCham-x-cor VCham-ex1) CHAM-Y)
                     JOY-GAUGE-POSN)
               CANVAS))
(check-expect (cham-walking VCham-ex2)
              (place-images
               (list (draw-cham-color (VCham-color VCham-ex2))
                     (draw-joy-level (VCham-joy VCham-ex2)))
               (list (make-posn (VCham-x-cor VCham-ex2) CHAM-Y)
                     JOY-GAUGE-POSN)
               CANVAS))
(check-expect (cham-walking VCham-ex3)
              (place-images
               (list (draw-cham-color (VCham-color VCham-ex3))
                     (draw-joy-level (VCham-joy VCham-ex3)))
               (list (make-posn (VCham-x-cor VCham-ex3) CHAM-Y)
                     JOY-GAUGE-POSN)
               CANVAS))
(check-expect (cham-walking VCham-ex4)
              (place-images
               (list (draw-cham-color (VCham-color VCham-ex4))
                     (draw-joy-level (VCham-joy VCham-ex4)))
               (list (make-posn (VCham-x-cor VCham-ex4) CHAM-Y)
                     JOY-GAUGE-POSN)
               CANVAS))

;; ChamColor -> Image
;; draws the chameleon according to the color given.
(define (draw-cham-color vch-c)
  (underlay (rectangle CHAM-WIDTH CHAM-HEIGHT "solid" vch-c)
            CHAM))

(check-expect (draw-cham-color (VCham-color VCham-ex0))
              (underlay (rectangle CHAM-WIDTH CHAM-HEIGHT "solid" "red")
                        CHAM))
(check-expect (draw-cham-color (VCham-color VCham-ex1))
              (underlay (rectangle CHAM-WIDTH CHAM-HEIGHT "solid" "red")
                        CHAM))
(check-expect (draw-cham-color (VCham-color VCham-ex2))
              (underlay (rectangle CHAM-WIDTH CHAM-HEIGHT "solid" "green")
                        CHAM))
(check-expect (draw-cham-color (VCham-color VCham-ex3))
              (underlay (rectangle CHAM-WIDTH CHAM-HEIGHT "solid" "blue")
                        CHAM))
(check-expect (draw-cham-color (VCham-color VCham-ex4))
              (underlay (rectangle CHAM-WIDTH CHAM-HEIGHT "solid" "red")
                        CHAM))

;; Joy -> Image
;; draws joy bar of chameleon according to the joy level.
(define (draw-joy-level vch-j)
  (underlay/align "left" "center"
                  (rectangle vch-j JOY-LEVEL-HEIGHT "solid" JOY-LEVEL-COLOR)
                  JOY-GAUGE))

(check-expect (draw-joy-level (VCham-joy VCham-ex0))
              (underlay/align "left" "center"
                              (rectangle JOY-MAX JOY-LEVEL-HEIGHT "solid" JOY-LEVEL-COLOR)
                              JOY-GAUGE))
(check-expect (draw-joy-level (VCham-joy VCham-ex1))
              (underlay/align "left" "center"
                              (rectangle (* 2/3 JOY-GAUGE-WIDTH) JOY-LEVEL-HEIGHT "solid" JOY-LEVEL-COLOR)
                              JOY-GAUGE))
(check-expect (draw-joy-level (VCham-joy VCham-ex2))
              (underlay/align "left" "center"
                              (rectangle (* 1/2 JOY-GAUGE-WIDTH) JOY-LEVEL-HEIGHT "solid" JOY-LEVEL-COLOR)
                              JOY-GAUGE))
(check-expect (draw-joy-level (VCham-joy VCham-ex3))
              (underlay/align "left" "center"
                              (rectangle (* 1/3 JOY-GAUGE-WIDTH) JOY-LEVEL-HEIGHT "solid" JOY-LEVEL-COLOR)
                              JOY-GAUGE))
(check-expect (draw-joy-level (VCham-joy VCham-ex4))
              (underlay/align "left" "center"
                              (rectangle JOY-MIN JOY-LEVEL-HEIGHT "solid" JOY-LEVEL-COLOR)
                              JOY-GAUGE))

;; VCham -> VCham
;; calculates chameleon's x-coordinate and joy level.
(define (cham-tantrum vch)
  (make-VCham (walks (VCham-x-cor vch))
              (joy-depletes (VCham-joy vch))
              (VCham-color vch)))

(check-expect (cham-tantrum VCham-ex0)
              (make-VCham (walks (VCham-x-cor VCham-ex0))
                          (joy-depletes (VCham-joy VCham-ex0))
                          (VCham-color VCham-ex0)))
(check-expect (cham-tantrum VCham-ex1)
              (make-VCham (walks (VCham-x-cor VCham-ex1))
                          (joy-depletes (VCham-joy VCham-ex1))
                          (VCham-color VCham-ex1)))
(check-expect (cham-tantrum VCham-ex2)
              (make-VCham (walks (VCham-x-cor VCham-ex2))
                          (joy-depletes (VCham-joy VCham-ex2))
                          (VCham-color VCham-ex2)))
(check-expect (cham-tantrum VCham-ex3)
              (make-VCham (walks (VCham-x-cor VCham-ex3))
                          (joy-depletes (VCham-joy VCham-ex3))
                          (VCham-color VCham-ex3)))
(check-expect (cham-tantrum VCham-ex4)
              (make-VCham (walks (VCham-x-cor VCham-ex4))
                          (joy-depletes (VCham-joy VCham-ex4))
                          (VCham-color VCham-ex4)))

;; XCor -> XCor
;; calculates chameleon's x-coordinate per tick; resets if
;; chameleon is out of frame.
(define (walks vch-x-c)
  (if (>= vch-x-c CHAM-RESET)
      CHAM-X-START
      (+ vch-x-c CHAM-VELOCITY)))

(check-expect (walks (VCham-x-cor VCham-ex0)) (+ CHAM-X-START CHAM-VELOCITY))
(check-expect (walks (VCham-x-cor VCham-ex1)) (+ (* 1/3 CANVAS-WIDTH) CHAM-VELOCITY))
(check-expect (walks (VCham-x-cor VCham-ex2)) (+ (* 1/2 CANVAS-WIDTH) CHAM-VELOCITY))
(check-expect (walks (VCham-x-cor VCham-ex3)) (+ (* 2/3 CANVAS-WIDTH) CHAM-VELOCITY))
(check-expect (walks (VCham-x-cor VCham-ex4)) CHAM-X-START)

;; Joy -> Joy
;; calculates chameleon's joy level per tick.
(define (joy-depletes vch-j)
  (if (<= vch-j JOY-MIN)
      JOY-MIN
      (- vch-j JOY/TICK)))

(check-within (joy-depletes (VCham-joy VCham-ex0)) (- JOY-MAX JOY/TICK) 0.01)
(check-within (joy-depletes (VCham-joy VCham-ex1)) (- (* 2/3 JOY-GAUGE-WIDTH) JOY/TICK) 0.01)
(check-within (joy-depletes (VCham-joy VCham-ex2)) (- (* 1/2 JOY-GAUGE-WIDTH) JOY/TICK) 0.01)
(check-within (joy-depletes (VCham-joy VCham-ex3)) (- (* 1/3 JOY-GAUGE-WIDTH) JOY/TICK) 0.01)
(check-within (joy-depletes (VCham-joy VCham-ex4)) JOY-MIN 0.01)

;; VCham KeyEvent -> VCham
;; does the following:
;; - "down" | raises chameleon's joy level by JOY/DOWN
;; - "r" | turns chameleon red
;; - "g" | turns chameleon green
;; - "b" | turns chameleon blue
(define (key-handler vch ke)
  (cond [(key=? ke "down") (joy-increases vch)]
        [(key=? ke "r") (color-change vch "red")]
        [(key=? ke "g") (color-change vch "green")]
        [(key=? ke "b") (color-change vch "blue")]
        [else vch]))

(check-expect (key-handler VCham-ex0 "down") (joy-increases VCham-ex0))
(check-expect (key-handler VCham-ex0 "r") (color-change VCham-ex0 "red"))
(check-expect (key-handler VCham-ex0 "g") (color-change VCham-ex0 "green"))
(check-expect (key-handler VCham-ex0 "b") (color-change VCham-ex0 "blue"))
(check-expect (key-handler VCham-ex0 "s") VCham-ex0)

;; VCham -> VCham
;; makes new VCham by increasing joy level by JOY/DOWN unless already full.
(define (joy-increases vch)
  (make-VCham (VCham-x-cor vch)
              (if (>= (VCham-joy vch) JOY-MAX)
                  JOY-MAX
                  (+ (VCham-joy vch) JOY/DOWN))
              (VCham-color vch)))

(check-expect (joy-increases VCham-ex0)
              (make-VCham (VCham-x-cor VCham-ex0)
                          JOY-MAX
                          (VCham-color VCham-ex0)))
(check-expect (joy-increases VCham-ex1)
              (make-VCham (VCham-x-cor VCham-ex1)
                          (+ (* 2/3 JOY-GAUGE-WIDTH) JOY/DOWN)
                          (VCham-color VCham-ex1)))
(check-expect (joy-increases VCham-ex2)
              (make-VCham (VCham-x-cor VCham-ex2)
                          (+ (* 1/2 JOY-GAUGE-WIDTH) JOY/DOWN)
                          (VCham-color VCham-ex2)))
(check-expect (joy-increases VCham-ex3)
              (make-VCham (VCham-x-cor VCham-ex3)
                          (+ (* 1/3 JOY-GAUGE-WIDTH) JOY/DOWN)
                          (VCham-color VCham-ex3)))
(check-expect (joy-increases VCham-ex4)
              (make-VCham (VCham-x-cor VCham-ex4)
                          (+ JOY-MIN JOY/DOWN)
                          (VCham-color VCham-ex4)))

;; VCham -> VCham
;; makes new VCham by changing color of the chameleon.
(define (color-change vch color)
  (make-VCham (VCham-x-cor vch)
              (VCham-joy vch)
              color))

(check-expect (color-change VCham-ex0 "blue")
              (make-VCham CHAM-X-START JOY-MAX "blue"))
(check-expect (color-change VCham-ex0 "green")
              (make-VCham CHAM-X-START JOY-MAX "green"))
(check-expect (color-change VCham-ex0 "red")
              (make-VCham CHAM-X-START JOY-MAX "red"))

;; stop-when
;; VCham -> Boolean
;; stops the program when the chameleon's joy is 0.
(define (joy-gone? vch)
  (<= (VCham-joy vch) JOY-MIN))

(check-expect (joy-gone? VCham-ex0) #f)
(check-expect (joy-gone? VCham-ex1) #f)
(check-expect (joy-gone? VCham-ex2) #f)
(check-expect (joy-gone? VCham-ex3) #f)
(check-expect (joy-gone? VCham-ex4) #t)

;; main function
(define (cham vcham)
  (big-bang vcham
            [to-draw cham-walking]
            [on-tick cham-tantrum]
            [on-key key-handler]
            [stop-when joy-gone?]))

(test)
(cham VCham-ex0)
