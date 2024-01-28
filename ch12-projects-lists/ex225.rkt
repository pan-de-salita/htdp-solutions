#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;;; constants and data definitions

(define TREE (bitmap "images/trees.png"))
(define TREE-WIDTH (image-width TREE))
(define TREE-HEIGHT (image-height TREE))

(define SCENE-WIDTH (* TREE-WIDTH 3))
(define SCENE-HEIGHT (* TREE-HEIGHT 2))

(define FOREST
  (beside TREE TREE TREE))
(define SCENE
  (overlay/align "center" "bottom"
                 FOREST (empty-scene SCENE-WIDTH SCENE-HEIGHT "dimgrey")))

(define HELICOPTER-LEFT (bitmap "images/helicopter-left.png"))
(define HELICOPTER-RIGHT (bitmap "images/helicopter-right.png"))
(define HELICOPTER-WIDTH (image-width HELICOPTER-LEFT))
(define HELICOPTER-HEIGHT (image-height HELICOPTER-LEFT))
(define HELICOPTER-VELOCITY 10)
(define HELICOPTER-LIMIT-LEFT (/ HELICOPTER-WIDTH 2))
(define HELICOPTER-LIMIT-RIGHT (- SCENE-WIDTH (/ HELICOPTER-WIDTH 2)))
(define HELICOPTER-Y-COORDINATE HELICOPTER-HEIGHT)

(define FIRE (bitmap "images/fire.png")) ;; same dimensions as DROPLET
(define FIRE-WIDTH (image-width FIRE))
(define FIRE-HEIGHT (image-height FIRE))
(define FIRE-LIMIT-LEFT HELICOPTER-LIMIT-LEFT)
(define FIRE-LIMIT-RIGHT HELICOPTER-LIMIT-RIGHT)
(define FIRE-LIMIT-TOP (round (- SCENE-HEIGHT (/ TREE-HEIGHT 2))))
(define FIRE-LIMIT-BOTTOM (round (+ (- SCENE-HEIGHT (/ FIRE-HEIGHT 2)) 1)))

(define DROPLET (bitmap "images/droplet.png"))
(define DROPLET-WIDTH (image-width DROPLET))
(define DROPLET-HEIGHT (image-height DROPLET))
(define DROPLET-VELOCITY 30)
(define DROPLET-Y-COORDINATE (+ HELICOPTER-Y-COORDINATE DROPLET-HEIGHT))

(define FIRE-EXTINGUISHABLE-RANGE (+ (/ FIRE-WIDTH 2) (/ DROPLET-WIDTH 2)))

(define TOO-MANY-FIRES 30)

(define GAME-OVER-TEXT " w e   l e t   t o o   m a n y   f i r e s   h a p p e n. ")
(define GAME-OVER-TEXT-FONT-SIZE 14)
(define GAME-OVER-TEXT-COLOR "white")
(define GAME-OVER-MESSAGE (text GAME-OVER-TEXT GAME-OVER-TEXT-FONT-SIZE GAME-OVER-TEXT-COLOR))
(define GAME-OVER-DIALOGUE
  (overlay GAME-OVER-MESSAGE
           (rectangle (image-width GAME-OVER-MESSAGE) (image-height GAME-OVER-MESSAGE)
                      "solid" "black")))

(define TIMER-INIT 0)
(define TIMER-FIRE-GENERATE 10)

(struct user [direction x-coordinate] #:transparent)
;; a User is a structure:
;;   (user String Number).
;; describes the following:
;; - the direction in which the helicopter is moving as controlled by the user
;; ;; - the x-coordinate of the helicopter as controlled by the user

(struct frame [helicopter fire droplet timer] #:transparent)
;; a Frame is a structure:
;;   (frame User List-of-Posns List-of-Posns Number).
;; describes the following:
;; - the direction and x-coordinate of the helicopter
;; - the Posn of each fire within the forest
;; - the Posn of each water droplet
;; - the time passed since the start of the game.

(define FRAME-INIT
  (frame (user "right" (/ SCENE-WIDTH 2))
         '() '() TIMER-INIT))

;; NOTE: a Frame is a Game-State.

;;;; functions

;; main function.

(define (forest-main game-state)
  (big-bang game-state
            [to-draw forest-render]
            [on-tick forest-move]
            [on-key forest-control]
            [stop-when game-over? game-over-render]))

;; Game-State -> Image
;; renders the image of the following onto SCENE:
;; - helicopter
;; - each fire
;; - each water droplet

(check-expect
 (forest-render
  (frame (user "right" (/ SCENE-WIDTH 2))
         '()
         '()
         56))
 (helicopter-render "right" (/ SCENE-WIDTH 2)
                    (fire-render '() (droplet-render '() SCENE))))
(check-expect
 (forest-render
  (frame (user "left" (/ SCENE-WIDTH 2))
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
               (make-posn 300 520)
               (make-posn 185 535))
         (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
               (make-posn (/ SCENE-WIDTH 4) 100)
               (make-posn 300 120)
               (make-posn 185 230))
         56))
 (helicopter-render "left" (/ SCENE-WIDTH 2)
                    (fire-render
                     (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
                           (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
                           (make-posn 300 520)
                           (make-posn 185 535))
                     (droplet-render
                      (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
                            (make-posn (/ SCENE-WIDTH 4) 100)
                            (make-posn 300 120)
                            (make-posn 185 230))
                      SCENE))))

(define (forest-render game-state)
  (helicopter-render (user-direction (frame-helicopter game-state))
                     (user-x-coordinate (frame-helicopter game-state))
                     (fire-render (frame-fire game-state)
                                  (droplet-render (frame-droplet game-state)
                                                  SCENE))))

;; Number String Image -> Image
;; renders the image of the helicopter onto a given image.

(check-expect
 (helicopter-render "left" (/ SCENE-WIDTH 2) SCENE)
 (place-image HELICOPTER-LEFT
              (/ SCENE-WIDTH 2) HELICOPTER-Y-COORDINATE
              SCENE))

(define (helicopter-render helicopter-direction helicopter-x-coordinate image)
  (place-image (if (string=? helicopter-direction "left") HELICOPTER-LEFT HELICOPTER-RIGHT)
               helicopter-x-coordinate HELICOPTER-Y-COORDINATE
               image))

;; List-of-Posns Image -> Image
;; renders the image of each fire onto a given image.

(check-expect (fire-render '() SCENE) SCENE)
(check-expect
 (fire-render
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP))
  SCENE)
 (place-images (make-list (length (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP))) FIRE)
               (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP))
               SCENE))
(check-expect
 (fire-render
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
        (make-posn 300 520)
        (make-posn 185 535))
  SCENE)
 (place-images (make-list
                (length
                 (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
                       (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
                       (make-posn 300 520)
                       (make-posn 185 535)))
                FIRE)
               (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
                     (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
                     (make-posn 300 520)
                     (make-posn 185 535))
               SCENE))

(define (fire-render fire-posns image)
  (cond [(empty? fire-posns) image]
        [else (place-images (make-list (length fire-posns) FIRE)
                            fire-posns
                            image)]))

;; List-of-Posns Image -> Image
;; renders the image of each droplet onto a given image.

(check-expect (droplet-render '() SCENE) SCENE)
(check-expect
 (droplet-render
  (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE))
  SCENE)
 (place-images (make-list (length (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE))) DROPLET)
               (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE))
               SCENE))
(check-expect
 (droplet-render
  (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
        (make-posn (/ SCENE-WIDTH 4) 100)
        (make-posn 300 120)
        (make-posn 185 230))
  SCENE)
 (place-images (make-list
                (length
                 (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
                       (make-posn (/ SCENE-WIDTH 4) 100)
                       (make-posn 300 120)
                       (make-posn 185 230)))
                DROPLET)
               (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
                     (make-posn (/ SCENE-WIDTH 4) 100)
                     (make-posn 300 120)
                     (make-posn 185 230))
               SCENE))

(define (droplet-render droplet-posns image)
  (cond [(empty? droplet-posns) image]
        [else (place-images (make-list (length droplet-posns) DROPLET)
                            droplet-posns
                            image)]))

;; Game-State -> Game-State
;; does the following per clock tick:
;; - determines the Posn of the next fire when the seconds passed is
;;   divisible by TIMER-FIRE-GENERATE
;; - extinguishes any fires when a droplet is close enough to it
;; - moves any droplet by DROPLET-VELOCITY
;; - increases timer by 1 second.

(check-random
 (forest-move
  (frame (user "right" (/ SCENE-WIDTH 2))
         '()
         '()
         0))
 (frame (user "right" (/ SCENE-WIDTH 2))
        (fire-move '() '() 0)
        (droplet-move '())
        (timer-move 0)))
(check-random
 (forest-move
  (frame (user "right" (/ SCENE-WIDTH 2))
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
               (make-posn 300 520)
               (make-posn 185 535))
         (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
               (make-posn (/ SCENE-WIDTH 4) 100)
               (make-posn 300 120)
               (make-posn 185 230))
         15))
 (frame (user "right" (/ SCENE-WIDTH 2))
        (fire-move
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
               (make-posn 300 520)
               (make-posn 185 535))
         (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
               (make-posn (/ SCENE-WIDTH 4) 100)
               (make-posn 300 120)
               (make-posn 185 230))
         15)
        (droplet-move
         (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
               (make-posn (/ SCENE-WIDTH 4) 100)
               (make-posn 300 120)
               (make-posn 185 230)))
        (timer-move 15)))
(check-random
 (forest-move
  (frame (user "right" (/ SCENE-WIDTH 2))
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
               (make-posn 300 520)
               (make-posn 185 535))
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) 200)
               (make-posn 300 120)
               (make-posn 185 528))
         15))
 (frame (user "right" (/ SCENE-WIDTH 2))
        (fire-move
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
               (make-posn 300 520)
               (make-posn 185 535))
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) 200)
               (make-posn 300 120)
               (make-posn 185 528))
         15)
        (droplet-move
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) 200)
               (make-posn 300 120)
               (make-posn 185 528)))
        (timer-move 15)))

(define (forest-move game-state)
  (frame (frame-helicopter game-state)
         (fire-move (frame-fire game-state)
                    (frame-droplet game-state)
                    (frame-timer game-state))
         (droplet-move (frame-droplet game-state))
         (timer-move (frame-timer game-state))))

;; List-of-Posns List-of-Posns Number -> List-of-Posns
;; does the following:
;; - determines the Posn of the next fire when the seconds passed is
;;   divisible by TIMER-FIRE-GENERATE
;; - extinguishes any fires when a droplet is close enough to it.

(check-random
 (fire-move '() '() 2)
 (fire-extinguish '() '()))
(check-random
 (fire-move '() '() 20)
 (cons (fire-generate '()) (fire-extinguish '() '())))
(check-random
 (fire-move
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
        (make-posn 300 520)
        (make-posn 185 535))
  (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
        (make-posn (/ SCENE-WIDTH 4) 100)
        (make-posn 300 120)
        (make-posn 185 230))
  100)
 (cons (fire-generate
        (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
              (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
              (make-posn 300 520)
              (make-posn 185 535)))
       (fire-extinguish
        (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
              (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
              (make-posn 300 520)
              (make-posn 185 535))
        (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
              (make-posn (/ SCENE-WIDTH 4) 100)
              (make-posn 300 120)
              (make-posn 185 230)))))
(check-random
 (fire-move
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
        (make-posn 300 520)
        (make-posn 185 535))
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) 200)
        (make-posn 300 120)
        (make-posn 185 528))
  100)
 (cons (fire-generate
        (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
              (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
              (make-posn 300 520)
              (make-posn 185 535)))
       (fire-extinguish
        (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
              (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
              (make-posn 300 520)
              (make-posn 185 535))
        (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
              (make-posn (/ SCENE-WIDTH 4) 200)
              (make-posn 300 120)
              (make-posn 185 528)))))

(define (fire-move fire-posns droplet-posns timer)
  (cond [(fire-generate? timer)
         (cons (fire-generate fire-posns) (fire-extinguish fire-posns droplet-posns))]
        [else (fire-extinguish fire-posns droplet-posns)]))

;; Number -> Boolean
;; checks if the seconds passed is divisible by TIMER-FIRE-GENERATE.

(check-expect (fire-generate? TIMER-FIRE-GENERATE) #t)
(check-expect (fire-generate? (+ TIMER-FIRE-GENERATE 1)) #f)

(define (fire-generate? timer)
  (= (modulo timer TIMER-FIRE-GENERATE) 0))

;; List-of-Posns -> Posn
;; generates a Posn for a new fire.

(check-random
 (fire-generate '())
 (fire-check-generate (make-posn (random FIRE-LIMIT-LEFT FIRE-LIMIT-RIGHT)
                                 (random FIRE-LIMIT-TOP FIRE-LIMIT-BOTTOM))
                      '()))
(check-random
 (fire-generate (list (make-posn 300 520)))
 (fire-check-generate (make-posn (random FIRE-LIMIT-LEFT FIRE-LIMIT-RIGHT)
                                 (random FIRE-LIMIT-TOP FIRE-LIMIT-BOTTOM))
                      (list (make-posn 300 520))))

(define (fire-generate fire-posns)
  (fire-check-generate (make-posn (random FIRE-LIMIT-LEFT FIRE-LIMIT-RIGHT)
                                  (random FIRE-LIMIT-TOP FIRE-LIMIT-BOTTOM))
                       fire-posns))

;; Posn List-of-Posns -> Posn
;; checks to see if the new Posn is equal to any of the existing fire Posns:
;; - if true, generates another new Posn
;; - else, returns the current new Posn.

(check-random
 (fire-check-generate
  (make-posn 300 520)
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
        (make-posn 300 520)
        (make-posn 185 535)))
 (fire-generate
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
        (make-posn 300 520)
        (make-posn 185 535))))
(check-expect
 (fire-check-generate
  (make-posn 305 527)
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
        (make-posn 300 520)
        (make-posn 185 535)))
 (make-posn 305 527))

(define (fire-check-generate posn-new fire-posns)
  (cond [(not (boolean? (member posn-new fire-posns))) (fire-generate fire-posns)]
        [else posn-new]))

;; List-of-Posns List-of-Posns -> List-of-Posns
;; checks if any fire is close enough to a droplet:
;; - if true, extinguishes said fire
;; - else, the fire is kept burning.

(check-expect
 (fire-extinguish
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
        (make-posn 300 520)
        (make-posn 185 535))
  (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
        (make-posn (/ SCENE-WIDTH 4) 100)
        (make-posn 300 120)
        (make-posn 185 230)))
 (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
       (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
       (make-posn 300 520)
       (make-posn 185 535)))
(check-expect
 (fire-extinguish
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
        (make-posn 300 520)
        (make-posn 185 535))
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) 200)
        (make-posn 300 120)
        (make-posn 185 528)))
 (list (make-posn 300 520)))

(define (fire-extinguish fire-posns droplet-posns)
  (cond [(empty? fire-posns) '()]
        [else (cond [(fire-within-extinguishable-range? (car fire-posns) droplet-posns)
                     (fire-extinguish (cdr fire-posns) droplet-posns)]
                    [else (cons (car fire-posns) (fire-extinguish (cdr fire-posns) droplet-posns))])]))

;; Posn List-of-Posns -> Boolean
;; checks if any fire is close enough to a droplet.

(check-expect
 (fire-within-extinguishable-range?
  (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
  (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
        (make-posn (/ SCENE-WIDTH 4) 100)
        (make-posn 300 120)
        (make-posn 185 230)))
 #f)
(check-expect
 (fire-within-extinguishable-range?
  (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) 100)
        (make-posn 300 120)
        (make-posn 185 230)))
 #t)

(define (fire-within-extinguishable-range? fire-posn droplet-posns)
  (cond [(empty? droplet-posns) #f]
        [else (or (<=
                   (sqrt (+ (sqr (- (posn-x fire-posn) (posn-x (car droplet-posns))))
                            (sqr (- (posn-y fire-posn) (posn-y (car droplet-posns))))))
                   FIRE-EXTINGUISHABLE-RANGE)
                  (fire-within-extinguishable-range? fire-posn (cdr droplet-posns)))]))

;; List-of-Posns -> List-of-Posns
;; moves any droplet by DROPLET-VELOCITY.

(check-expect (droplet-move '()) '())
(check-expect
 (droplet-move
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) 100)
        (make-posn 300 120)
        (make-posn 185 230)))
 (cons (make-posn (/ SCENE-WIDTH 2)
                  (+ FIRE-LIMIT-TOP DROPLET-VELOCITY))
       (droplet-move
        (list (make-posn (/ SCENE-WIDTH 4) 100)
              (make-posn 300 120)
              (make-posn 185 230)))))

(define (droplet-move droplet-posns)
  (cond [(empty? droplet-posns) '()]
        [else (cons (make-posn (posn-x (car droplet-posns))
                               (+ (posn-y (car droplet-posns)) DROPLET-VELOCITY))
                    (droplet-move (cdr droplet-posns)))]))

;; Number -> Number
;; adds a second to the time passed.

(check-expect (timer-move 3) 4)

(define (timer-move timer)
  (add1 timer))

;; Game-State Key-Event -> Game-State
;; does the following depending on Key-Event:
;; - if the user inputs "left" or "right", the helicopter moves left
;;   or right by HELICOPTER-VELOCITY
;; - if the user inputs " ", a droplet is launched from the helicopter's
;;   current Posn.

(check-expect
 (forest-control
  (frame (user "right" (/ SCENE-WIDTH 2))
         '()
         '()
         0)
  "left")
 (frame (helicopter-move (helicopter-control (user "right" (/ SCENE-WIDTH 2)) "left"))
        '()
        (droplet-generate '() (/ SCENE-WIDTH 2) "left")
        0))
(check-expect
 (forest-control
  (frame (user "right" (/ SCENE-WIDTH 2))
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
               (make-posn 300 520)
               (make-posn 185 535))
         (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
               (make-posn (/ SCENE-WIDTH 4) 100)
               (make-posn 300 120)
               (make-posn 185 230))
         15)
  "right")
 (frame (helicopter-move (helicopter-control (user "right" (/ SCENE-WIDTH 2)) "right"))
        (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
              (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
              (make-posn 300 520)
              (make-posn 185 535))
        (droplet-generate
         (list (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
               (make-posn (/ SCENE-WIDTH 4) 100)
               (make-posn 300 120)
               (make-posn 185 230))
         (/ SCENE-WIDTH 2)
         "right")
        15))
(check-expect
 (forest-control
  (frame (user "right" (/ SCENE-WIDTH 2))
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
              (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
              (make-posn 300 520)
              (make-posn 185 535))
        (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
              (make-posn (/ SCENE-WIDTH 4) 200)
              (make-posn 300 120)
              (make-posn 185 528))
         15)
  " ")
 (frame (helicopter-move (helicopter-control (user "right" (/ SCENE-WIDTH 2)) " "))
        (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
              (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
              (make-posn 300 520)
              (make-posn 185 535))
        (droplet-generate
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) 200)
               (make-posn 300 120)
               (make-posn 185 528))
         (/ SCENE-WIDTH 2)
         " ")
        15))

(define (forest-control game-state key-event)
  (frame (helicopter-move (helicopter-control (frame-helicopter game-state) key-event))
         (frame-fire game-state)
         (droplet-generate (frame-droplet game-state) (user-x-coordinate (frame-helicopter game-state)) key-event)
         (frame-timer game-state)))

;; User Key-Event -> User
;; changes the direction of the helicopter depending on user input:
;; - if the user inputs "left", the helicopter faces left
;; - if the user inputs "right", the helicopter faces right
;; - else, the helicopter's direction does not change.

(check-expect
 (helicopter-control (user "right" (/ SCENE-WIDTH 2)) "left")
 (user "left" (/ SCENE-WIDTH 2)))
(check-expect
 (helicopter-control (user "right" (/ SCENE-WIDTH 2)) "right")
 (user "right" (/ SCENE-WIDTH 2)))
(check-expect
 (helicopter-control (user "right" (/ SCENE-WIDTH 2)) "o")
 (user "right" (/ SCENE-WIDTH 2)))

(define (helicopter-control helicopter key-event)
  (user (cond [(key=? key-event "left") "left"]
              [(key=? key-event "right")"right"]
              [else (user-direction helicopter)])
        (user-x-coordinate helicopter)))

;; User -> User
;; moves the helicopter by HELICOPTER-VELOCITY to the left
;; or to the right.

(check-expect
 (helicopter-move (user "right" (/ SCENE-WIDTH 2)))
 (user "right" (+ (/ SCENE-WIDTH 2) HELICOPTER-VELOCITY)))
(check-expect
 (helicopter-move (user "left" (/ SCENE-WIDTH 2)))
 (user "left" (- (/ SCENE-WIDTH 2) HELICOPTER-VELOCITY)))
(check-expect
 (helicopter-move (user "left" HELICOPTER-LIMIT-LEFT))
 (user "left" HELICOPTER-LIMIT-LEFT))
(check-expect
 (helicopter-move (user "right" HELICOPTER-LIMIT-RIGHT))
 (user "right" HELICOPTER-LIMIT-RIGHT))

(define (helicopter-move helicopter)
  (user (user-direction helicopter)
        (cond [(and (string=? (user-direction helicopter) "left")
                    (> (user-x-coordinate helicopter) HELICOPTER-LIMIT-LEFT))
               (- (user-x-coordinate helicopter) HELICOPTER-VELOCITY)]
              [(and (string=? (user-direction helicopter) "right")
                    (< (user-x-coordinate helicopter) HELICOPTER-LIMIT-RIGHT))
               (+ (user-x-coordinate helicopter) HELICOPTER-VELOCITY)]
              [else (user-x-coordinate helicopter)])))

;; List-of-Posns Number Key-Event -> List-of-Posns
;; launches a droplet from the helicopter's current Posn if the user
;; inputs " ".

(check-expect
 (droplet-generate '() (/ SCENE-WIDTH 2) " ")
 (cons (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE) '()))
(check-expect
 (droplet-generate
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) 100)
        (make-posn 300 120)
        (make-posn 185 230))
  (/ SCENE-WIDTH 2)
  " ")
 (cons (make-posn (/ SCENE-WIDTH 2) DROPLET-Y-COORDINATE)
       (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
             (make-posn (/ SCENE-WIDTH 4) 100)
             (make-posn 300 120)
             (make-posn 185 230))))
(check-expect
 (droplet-generate
  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
        (make-posn (/ SCENE-WIDTH 4) 100)
        (make-posn 300 120)
        (make-posn 185 230))
  (/ SCENE-WIDTH 2)
  "left")
 (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
       (make-posn (/ SCENE-WIDTH 4) 100)
       (make-posn 300 120)
       (make-posn 185 230)))

(define (droplet-generate droplet-posns helicopter-x-coordinate key-event)
  (cond [(key=? key-event " ")
         (cons (make-posn helicopter-x-coordinate DROPLET-Y-COORDINATE) droplet-posns)]
        [else droplet-posns]))

;; Game-State -> Boolean
;; checks if there are too many fires, in which case the
;; game ends.

(check-expect
 (game-over?
  (frame (user "right" (/ SCENE-WIDTH 2))
         (make-list 3 (fire-generate '()))
         '()
         20))
 #f)
(check-expect
 (game-over?
  (frame (user "right" (/ SCENE-WIDTH 2))
         (make-list TOO-MANY-FIRES (fire-generate '()))
         '()
         20))
 #t)

(define (game-over? game-state)
  (>= (length (frame-fire game-state)) TOO-MANY-FIRES))

;; Game-State -> Image
;; renders the final frame of the game.

(check-expect
 (game-over-render
  (frame (user "right" (/ SCENE-WIDTH 2))
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
               (make-posn 300 520)
               (make-posn 185 535))
         (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
               (make-posn (/ SCENE-WIDTH 4) 200)
               (make-posn 300 120)
               (make-posn 185 528))
         15))
 (overlay GAME-OVER-DIALOGUE
          (forest-render
           (frame (user "right" (/ SCENE-WIDTH 2))
                  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
                        (make-posn (/ SCENE-WIDTH 4) FIRE-LIMIT-BOTTOM)
                        (make-posn 300 520)
                        (make-posn 185 535))
                  (list (make-posn (/ SCENE-WIDTH 2) FIRE-LIMIT-TOP)
                        (make-posn (/ SCENE-WIDTH 4) 200)
                        (make-posn 300 120)
                        (make-posn 185 528))
                  15))))

(define (game-over-render game-state)
  (overlay GAME-OVER-DIALOGUE
           (forest-render game-state)))

;;;; applicaton

(test)
(forest-main FRAME-INIT)
