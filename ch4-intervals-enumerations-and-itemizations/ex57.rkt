;; ex. 57
;; recall that the word "height" forced us to choose one of two possible
;; interpretations. now that you have solved the exercises in this section, solve them again
;; using the first interpretaton of the word. compare and contrast the solutions.
;; first interpretation: the word "height" could refer to the distance between the ground
;; and the rocket's point of reference.

#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

(define HEIGHT 300) ;; distances in pixels
(define WIDTH 100)
(define YDELTA 3)

(define BACKG (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))

(define CENTER (/ (image-height ROCKET) 2))
(define GROUNDED CENTER) ;; starting distance between ROCKET and ground

;; an LRCD (for launching rocket countdown) is one of:
;; - "resting" | rocket grounded
;; - a Number between -3 and -1 | rocket in countdown mode
;; - a NonnegativeNumber | the number of pixels between the top
;; of the canvas and the rocket (its height)

;; LRCD -> Image
;; places ROCKET into BACKG according to LRCD; displays countdown
;; when rocket is ready for launch
(check-expect (render-world "resting") (render-rocket GROUNDED))
(check-expect (render-world -3) (place-image (text (number->string -3) 14 "red")
                                             10 (* 3/4 WIDTH)
                                             (render-rocket GROUNDED)))
(check-expect (render-world 14) (render-rocket (+ CENTER 14)))

(define (render-world x)
  (cond [(and (string? x) (string=? x "resting")) (render-rocket GROUNDED)]
        [(<= -3 x -1) (place-image (text (number->string x) 14 "red")
                                   10 (* 3/4 WIDTH)
                                   (render-rocket GROUNDED))]
        [(>= x 0) (render-rocket (+ CENTER x))]))

;; Number -> Image
;; auxiliary function that renders ROCKET at appropriate height
(check-expect (render-rocket 14) (place-image ROCKET 10 (- HEIGHT 14) BACKG))

(define (render-rocket dist-from-ground)
  (place-image ROCKET 10 (- HEIGHT dist-from-ground) BACKG))

;; LRCD -> LRCD
;; starts countdown and then launches ROCKET when user presses spacebar
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -3 "a") -3)
(check-expect (launch 14 " ") 14)
(check-expect (launch 14 "a") 14)

(define (launch x ke)
  (cond [(and (string? x) (string=? x "resting"))
         (if (string=? ke " ") -3 x)]
        [(<= -3 x -1) x]
        [(>= x 0) x]))

;; LRCD -> LRCD
;; raises ROCKET by YDELTA per clock tick if already launched
(check-expect (ascend "resting") "resting")
(check-expect (ascend -3) -2)
(check-expect (ascend -2) -1)
(check-expect (ascend -1) 0)
(check-expect (ascend 14) (+ 14 YDELTA))

(define (ascend x)
  (cond [(and (string? x) (string=? x "resting")) x]
        [(<= -3 x -1) (if (= x -1) 0 (+ x 1))]
        [(>= x 0) (+ x YDELTA)]))

;; LRCD -> BOOLEAN
;; stops the program when ROCKET is out of frame
(check-expect (out-of-frame? "resting") #f)
(check-expect (out-of-frame? -1) #f)
(check-expect (out-of-frame? 14) #f)
(check-expect (out-of-frame? HEIGHT) #f)
(check-expect (out-of-frame? (+ HEIGHT YDELTA)) #t)

(define (out-of-frame? x)
  (and (number? x) (> x HEIGHT)))

;; LRCD -> LRCD
(define (main x)
  (big-bang x
            [to-draw render-world]
            [on-key launch]
            [on-tick ascend 0.314]
            [stop-when out-of-frame?]))

(test)

;; GROUNDED initially held the value (- HEIGHT CENTER), which amounted to 285,
;; and didn't correlate with the interpretation of "height" being the distance
;; between the ground and ROCKET. changed GROUNDED to CENTER, that is, 15, and
;; modified render-rocket to subtract LRCD (when it was a NonnegativeNumber) from
;; HEIGHT. this matches our interpretation of height better as the number grows
;; when LRCD passes through the ascend function.
