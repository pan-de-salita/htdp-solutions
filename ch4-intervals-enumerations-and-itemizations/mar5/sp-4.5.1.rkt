;; sample problem 4.5.1
;; design a program that launches a rocket when the user of your program presses
;; the space bar. at that point, the simulation starts a countdown for three ticks,
;; before it displays the scenery of a rising rocket. the rocket should move upward
;; at a rate of three pixels per clock tick.

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
(define GROUNDED (- HEIGHT CENTER))

;; an LRCD (for launching rocket countdown) is one of:
;; - "resting" | rocket grounded
;; - a Number between -3 and -1 | rocket in countdown mode
;; - a NonnegativeNumber | the number of pixels between the top
;; of the canvas and the rocket (its height)

;; LRCD -> Image
;; places ROCKET into BACKG according to LRCD; displays countdown
;; when rocket is ready for launch
(check-expect
 (render-world "resting")
 (place-image ROCKET 10 GROUNDED BACKG))
(check-expect
 (render-world -3)
 (place-image (text (number->string -3) 14 "red")
              10 (* 3/4 WIDTH)
              (place-image ROCKET 10 GROUNDED BACKG)))
(check-expect
 (render-world 14)
 (place-image ROCKET 10 (- 14 CENTER) BACKG))

(define (render-world x)
  (cond [(and (string? x) (string=? x "resting")) (render-rocket GROUNDED)]
        [(<= -3 x -1) (place-image (text (number->string x) 14 "red")
                                   10 (* 3/4 WIDTH)
                                   (render-rocket GROUNDED))]
        [(>= x 0) (render-rocket (- x CENTER))]))

;; Number -> Image
;; auxiliary function that renders ROCKET at appropriate height
(check-expect (render-rocket 14) (place-image ROCKET 10 14 BACKG))

(define (render-rocket x)
  (place-image ROCKET 10 x BACKG))

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
(check-expect (ascend -1) HEIGHT)
(check-expect (ascend HEIGHT) (- HEIGHT YDELTA))

(define (ascend x)
  (cond [(and (string? x) (string=? x "resting")) x]
        [(<= -3 x -1) (if (= x -1) HEIGHT (+ x 1))]
        [(>= x 0) (- x YDELTA)]))

;; LRCD -> BOOLEAN
;; stops the program when ROCKET is out of frame
(check-expect (out-of-frame? "resting") #f)
(check-expect (out-of-frame? -1) #f)
(check-expect (out-of-frame? 14) #f)
(check-expect (out-of-frame? 0) #t)

(define (out-of-frame? x)
  (and (number? x) (= x 0)))

;; LRCD -> LRCD
(define (main x)
  (big-bang x
            [to-draw render-world]
            [on-key launch]
            [on-tick ascend 0.314]
            [stop-when out-of-frame?]))

(test)

;; original render-rocket function. replaced with above render-rocket function
;; to improve readability for user when function is used in render-world
;; ((render-rocket GROUNDED) is more readable than (render-rocket x)).
;;
;; LRCD -> Image
;; renders ROCKET according to LRCD
;; (check-expect (render-rocket "resting") (place-image ROCKET 10 GROUNDED BACKG))
;; (check-expect (render-rocket -3) (place-image ROCKET 10 GROUNDED BACKG))
;; (check-expect (render-rocket 14) (place-image ROCKET (- 14 CENTER) BACKG))
;;
;; (define (render-rocket x)
;;   (place-image ROCKET
;;                10
;;                (cond [(and (string? x) (string=? x "resting"))
;;                       GROUNDED]
;;                      [(<= -3 x -1)
;;                       GROUNDED]
;;                      [(>= x 0)
;;                       (- x CENTER)])
;;                BACKG))
