#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;; constants and data definitions

(define CANVAS-WIDTH 450)
(define CANVAS-HEIGHT 225)
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT "darkgrey"))

(define CHAM (bitmap "./cham-0.png"))
(define CHAM-WIDTH (image-width CHAM))
(define CHAM-HEIGHT (image-height CHAM))
(define CHAM-X-START (- (/ CHAM-WIDTH 2)))
(define CHAM-RESET (+ CANVAS-WIDTH (/ CHAM-WIDTH 2)))
(define CHAM-DEFAULT-COLOR "green")

(define CAT-0 (bitmap "./cat-0.png"))
(define CAT-1 (bitmap "./cat-1.png"))
(define CAT-WIDTH (image-width CAT-0))
(define CAT-HEIGHT (image-height CAT-0))
(define CAT-X-START (- (/ CAT-WIDTH 2)))
(define CAT-RESET (+ CANVAS-WIDTH (/ CAT-WIDTH 2)))

(define PET-Y (/ CANVAS-HEIGHT 2))
(define PET-VELOCITY 3)

(define JOY/TICK 0.65)
(define JOY/DOWN 2)
(define JOY-GAUGE-WIDTH (* CANVAS-WIDTH 0.5))
(define JOY-GAUGE-HEIGHT (* CANVAS-HEIGHT 0.08))
(define JOY-GAUGE-X (/ CANVAS-WIDTH 2))
(define JOY-GAUGE-Y (- CANVAS-HEIGHT (* JOY-GAUGE-HEIGHT 1.5)))
(define JOY-LEVEL-COLOR "black")
(define JOY-LEVEL-HEIGHT JOY-GAUGE-HEIGHT)
(define JOY-MAX JOY-GAUGE-WIDTH)
(define JOY-MIN 0)
(define JOY-GAUGE (rectangle JOY-GAUGE-WIDTH JOY-GAUGE-HEIGHT "outline" "black"))
(define JOY-GAUGE-POSN (make-posn JOY-GAUGE-X JOY-GAUGE-Y))

;; an XCor is a Number.
;; represents the x-coordinate of a VAnimal as it walks
;; across canvas.

;; a Joy is a Number.
;; represents the joy level of a VAnimal.

;; a ChamColor is one of the following:
;; - "red"
;; - "green"
;; - "blue"
;; represetnts the color of the chameleon.

;; VCham is a structure:
;;     (make-VCham XCor Joy ChamColor)
;; (make-VCham xc j cc) describes a chameleon's:
;; - x-coordinate
;; - joy
;; - color
(define-struct VCham [x-cor joy color])

(define VCham-test-0 (make-VCham CHAM-X-START JOY-MAX "red"))
(define VCham-test-1 (make-VCham (* 1/3 CANVAS-WIDTH) (* 2/3 JOY-GAUGE-WIDTH) "red"))
(define VCham-test-2 (make-VCham CHAM-RESET JOY-MIN "red"))

;; VCat is a structure:
;;      (make-VCat XCor Joy)
;; (make-VCat xcor j) describes a cat's:
;; - x-coordinate
;; - joy
(define-struct VCat [x-cor joy])

(define VCat-test-0 (make-VCat CAT-X-START JOY-MAX))
(define VCat-test-1 (make-VCat (* 1/2 CANVAS-WIDTH) (* 1/2 JOY-GAUGE-WIDTH)))
(define VCat-test-2 (make-VCat CAT-RESET JOY-MIN))

;; A VAnimal is either:
;; - a VCat
;; - a VCham

;;; functions

;; VAnimal -> Image
;; places VAnimal onto CANVAS
(check-expect (pet->image VCham-test-0)
              (place-images
               (list CHAM
                     (joy-level-of VCham-test-0))
               (list (posn-of VCham-test-0)
                     JOY-GAUGE-POSN)
               CANVAS))
(check-expect (pet->image VCat-test-0)
              (place-images
               (list CAT-0
                     (joy-level-of VCat-test-0))
               (list (posn-of VCat-test-0)
                     JOY-GAUGE-POSN)
               CANVAS))

(define (pet->image animal)
  (place-images
   (list (which animal)
         (joy-level-of animal))
   (list (posn-of animal)
         JOY-GAUGE-POSN)
   CANVAS))

;; VAnimal -> Image
;; returns either CAT or CHAM depending on VAnimal.
(check-expect (which VCham-test-0) (color VCham-test-0))
(check-expect (which VCat-test-0) (animate VCat-test-0))

(define (which animal)
  (cond [(VCham? animal) (color animal)]
        [(VCat? animal) (animate animal)]))

;; VCham -> Image
;; colors CHAM.
(check-expect (color VCham-test-0)
              (overlay/align "middle" "middle"
                             CHAM
                             (rectangle CHAM-WIDTH CHAM-HEIGHT
                                        "solid" (VCham-color VCham-test-0))))
(check-expect (color VCham-test-1)
              (overlay/align "middle" "middle"
                             CHAM
                             (rectangle CHAM-WIDTH CHAM-HEIGHT
                                        "solid" (VCham-color VCham-test-1))))

(define (color cham)
  (overlay/align "middle" "middle"
                 CHAM
                 (rectangle CHAM-WIDTH CHAM-HEIGHT
                            "solid" (VCham-color cham))))

;; VCat -> Image
;; switches between the following depending on x-coordinate:
;; - x-coordinate divisible by 2 | return CAT-0
;; - x-coordinate indivisible by 2 | return CAT-1
(check-expect (animate VCat-test-0) CAT-0)
(check-expect (animate VCat-test-1) CAT-1)

(define (animate cat)
  (if (= (modulo (VCat-x-cor cat) 2) 0)
      CAT-0
      CAT-1))

;; VAnimal -> Image
;; renders joy level of given VAnimal.
(check-expect (joy-level-of VCham-test-0)
              (underlay/align "left" "middle"
                              (rectangle
                               (VCham-joy VCham-test-0)
                               JOY-GAUGE-HEIGHT
                               "solid" "black")
                              JOY-GAUGE))
(check-expect (joy-level-of VCat-test-0)
              (underlay/align "left" "middle"
                              (rectangle
                               (VCat-joy VCat-test-0)
                               JOY-GAUGE-HEIGHT
                               "solid" "black")
                               JOY-GAUGE))

(define (joy-level-of animal)
  (underlay/align "left" "middle"
                  (rectangle
                   (cond [(and (VCham? animal) (< (VCham-joy animal) JOY-MAX)) (VCham-joy animal)]
                         [(and (VCat? animal) (< (VCat-joy animal) JOY-MAX)) (VCat-joy animal)]
                         [else JOY-MAX])
                   JOY-GAUGE-HEIGHT
                   "solid" "black")
                  JOY-GAUGE))

;; VAnimal -> Posn
;; computes for the Posn of given VAnimal
(check-expect (posn-of VCham-test-0)
              (make-posn (VCham-x-cor VCham-test-0)
                         PET-Y))
(check-expect (posn-of VCat-test-0)
              (make-posn (VCat-x-cor VCat-test-0)
                         PET-Y))

(define (posn-of animal)
  (make-posn (cond [(VCham? animal) (VCham-x-cor animal)]
                   [(VCat? animal) (VCat-x-cor animal)])
             PET-Y))

;; VAnimal -> VAnimal
;; computes for the next x-coordinate and joy level of VAnimal.
(check-expect (next-state-of VCham-test-0)
              (make-VCham (+ (VCham-x-cor VCham-test-0) PET-VELOCITY)
                          (VCham-joy VCham-test-0)
                          (VCham-color VCham-test-0)))
(check-expect (next-state-of VCat-test-0)
              (make-VCat (+ (VCat-x-cor VCat-test-0) PET-VELOCITY)
                         (VCat-joy VCat-test-0)))

(define (next-state-of animal)
  (cond [(VCham? animal)
         (make-VCham (if (not (>= (VCham-x-cor animal) CHAM-RESET))
                         (+ (VCham-x-cor animal) PET-VELOCITY)
                         CHAM-X-START)
                     (- (VCham-joy animal) JOY/TICK)
                     (VCham-color animal))]
        [(VCat? animal)
         (make-VCat (if (not (>= (VCat-x-cor animal) CAT-RESET))
                        (+ (VCat-x-cor animal) PET-VELOCITY)
                        CAT-X-START)
                    (- (VCat-joy animal) JOY/TICK))]))

;; VAnimal KeyEvent -> VAnimal
;; changes VAnimal in the following ways:
;; - "k" | summons the cat
;; - "l" | summons the lizard
;; - "down" | feeds either the cat or the lizard
;; - "r" | changes the color of the lizard to red
;; - "g" | changes the color of the lizard to green
;; - "b" | changes the color of the lizard to blue
(check-expect (pet-controller VCham-test-1 "r") (change-color VCham-test-0 "r"))
(check-expect (pet-controller VCham-test-1 "g") (change-color VCham-test-0 "g"))
(check-expect (pet-controller VCham-test-1 "b") (change-color VCham-test-0 "b"))
(check-expect (pet-controller VCham-test-1 "down") (raise-joy-of-cham VCham-test-1))
(check-expect (pet-controller VCham-test-1 "k") (summon-cat VCham-test-1))
(check-expect (pet-controller VCham-test-1 "l") VCham-test-1)
(check-expect (pet-controller VCat-test-1 "down") (raise-joy-of-cat VCat-test-1))
(check-expect (pet-controller VCat-test-1 "l") (summon-lizard VCat-test-1))
(check-expect (pet-controller VCat-test-1 "k") VCat-test-1)

(define (pet-controller animal key-event)
  (cond [(VCham? animal)
         (cond [(or (key=? key-event "r")
                    (key=? key-event "g")
                    (key=? key-event "b"))
                (change-color animal key-event)]
               [(key=? key-event "down")
                (raise-joy-of-cham animal)]
               [(key=? key-event "k")
                (summon-cat animal)]
               [else animal])]
        [(VCat? animal)
         (cond [(key=? key-event "down")
                (raise-joy-of-cat animal)]
               [(key=? key-event "l")
                (summon-lizard animal)]
               [else animal])]))

;; VCham KeyEvent -> VCham
;; changes color of chameleon.
(check-expect (change-color VCham-test-1 "r")
              (make-VCham (VCham-x-cor VCham-test-1)
                          (VCham-joy VCham-test-1)
                          "red"))
(check-expect (change-color VCham-test-1 "g")
              (make-VCham (VCham-x-cor VCham-test-1)
                          (VCham-joy VCham-test-1)
                          "green"))
(check-expect (change-color VCham-test-1 "b")
              (make-VCham (VCham-x-cor VCham-test-1)
                          (VCham-joy VCham-test-1)
                          "blue"))

(define (change-color cham key-event)
  (make-VCham (VCham-x-cor cham)
              (VCham-joy cham)
              (cond [(key=? key-event "r") "red"]
                    [(key=? key-event "g") "green"]
                    [(key=? key-event "b") "blue"])))

;; VCham -> VCham
;; raises joy level of chameleon by JOY/DOWN
(check-expect (raise-joy-of-cham VCham-test-1)
              (make-VCham (VCham-x-cor VCham-test-1)
                          (+ (VCham-joy VCham-test-1) JOY/DOWN)
                          (VCham-color VCham-test-1)))

(define (raise-joy-of-cham cham)
  (make-VCham (VCham-x-cor cham)
              (+ (VCham-joy cham) JOY/DOWN)
              (VCham-color cham)))

;; VCham -> VCat
;; replaces chameleon with cat at x-coordinate, with
;; same joy level.
(check-expect (summon-cat VCham-test-1)
              (make-VCat (VCham-x-cor VCham-test-1)
                         (VCham-joy VCham-test-1)))

(define (summon-cat cham)
  (make-VCat (VCham-x-cor cham)
             (VCham-joy cham)))

;; VCat -> VCat
;; raises joy level of cat by JOY/DOWN.
(check-expect (raise-joy-of-cat VCat-test-1)
              (make-VCat (VCat-x-cor VCat-test-1)
                         (+ (VCat-joy VCat-test-1) JOY/DOWN)))

(define (raise-joy-of-cat cat)
  (make-VCat (VCat-x-cor cat)
             (+ (VCat-joy cat) JOY/DOWN)))

;; VCat -> VCham
;; replaces cat with chameleion at x-coordinate, with
;; same joy level.
(check-expect (summon-lizard VCat-test-1)
              (make-VCham (VCat-x-cor VCat-test-1)
                          (VCat-joy VCat-test-1)
                          CHAM-DEFAULT-COLOR))

(define (summon-lizard cat)
  (make-VCham (VCat-x-cor cat)
              (VCat-joy cat)
              CHAM-DEFAULT-COLOR))

;; VAnimal -> Boolean
;; stops program when the joy level of a pet reaches
;; JOY-MIN.
(check-expect (fed-up? VCham-test-0) #f)
(check-expect (fed-up? VCham-test-2) #t)
(check-expect (fed-up? VCat-test-0) #f)
(check-expect (fed-up? VCat-test-2) #t)

(define (fed-up? animal)
  (or (and (VCham? animal) (<= (VCham-joy animal) JOY-MIN))
      (and (VCat? animal) (<= (VCat-joy animal) JOY-MIN))))

;; Any -> VAnimal
;; is va an example of the VAnimal collection
(check-expect (v-animal? VCat-test-0) #t)
(check-expect (v-animal? VCham-test-0) #t)
(check-expect (v-animal? #t) #f)
(check-expect (v-animal? "potato") #f)
(check-expect (v-animal? 23) #f)
(check-expect (v-animal? empty-image) #f)

(define (v-animal? va)
  (or (VCat? va) (VCham? va)))

;; VAnimal -> VAnimal
;; main function.
(define (cham-cat animal)
  (big-bang animal
            [to-draw pet->image]
            [on-tick next-state-of]
            [on-key pet-controller]
            [stop-when fed-up?]))

;; application
;; (cham-cat VCham-test-0)
(test)
