#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)
(require racket/struct)

;;;; constants and data definitions

(define UFO (bitmap "images/ufo.png")) ;; 50x50 px
(define UFO-WIDTH (image-width UFO))
(define UFO-HEIGHT (image-height UFO))
(define UFO-VELOCITY-LANDING 2)
(define UFO-VELOCITY-RETREATING 40)

(define CANNON (bitmap "images/canon.png")) ;; same dimensions as UFO
(define CANNON-WIDTH (image-width CANNON))
(define CANNON-HEIGHT (image-height CANNON))
(define CANNON-VELOCITY 30)

(define MISSILE (bitmap "images/missile.png")) ;; 25x25 px
(define MISSILE-WIDTH (image-width MISSILE))
(define MISSILE-HEIGHT (image-height MISSILE))
(define MISSILE-VELOCITY 50)

(define SCENE-WIDTH (* UFO-HEIGHT 8))
(define SCENE-HEIGHT (* UFO-WIDTH 16))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT "dimgrey"))

(define UFO-LIMIT-LEFT (/ UFO-WIDTH 2))
(define UFO-LIMIT-RIGHT (- SCENE-WIDTH (/ UFO-WIDTH 2)))
(define CANNON-LIMIT-LEFT (/ CANNON-WIDTH 2))
(define CANNON-LIMIT-RIGHT (- SCENE-WIDTH (/ CANNON-WIDTH 2)))

(define UFO-POSN-INIT (make-posn (/ SCENE-WIDTH 2) (/ SCENE-HEIGHT 4)))
(define CANNON-Y (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
(define CANNON-X-INIT (/ SCENE-WIDTH 2))
(define MISSILES-Y-INIT (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))

(define UFO-WARP-RANGE-MIN 1)
(define UFO-WARP-RANGE-MAX 30)
(define UFO-ADJUSTMENT-X 30)

(define UFO-HIT-RANGE (+ (/ UFO-WIDTH 2) (/ MISSILE-WIDTH 2)))

(define UFO-RETREATED (- 0 (/ UFO-HEIGHT 2)))
(define UFO-LANDED (- SCENE-HEIGHT (/ UFO-HEIGHT 2)))

(struct frame [ufo cannon missiles] #:transparent)
;; a Frame is a structure:
;;   (frame Posn Number List-of-Posns);
;; one instance of which describes the following:
;; - a UFO's Posn
;; - a cannon's x-coordinate
;; - the Posns of every missile fired.
(define FRAME-INIT
  (frame UFO-POSN-INIT CANNON-X-INIT '()))

;; NOTE: a Game-State is a Frame.

;;;; functions

;; main function.

(define (ufo-main game-state)
  (big-bang game-state
            [to-draw game-render]
            [on-tick game-move]
            [on-key game-control]
            [stop-when game-over? game-over-render]))

;; Game-State -> Image
;; renders the following images onto SCENE:
;; - UFO
;; - CANNON
;; - MISSILEs (if fired).

(check-expect (game-render FRAME-INIT) (game-render/aiming UFO-POSN-INIT CANNON-X-INIT))
(check-expect
 (game-render
  (frame (make-posn 148 89)
         23
         (list (make-posn 29 MISSILES-Y-INIT)
               (make-posn 100 158)
               (make-posn 122 95))))
 (game-render/fired (make-posn 148 89)
                    23
                    (list (make-posn 29 MISSILES-Y-INIT)
                          (make-posn 100 158)
                          (make-posn 122 95))))

(define (game-render game-state)
  (cond [(aiming? (frame-missiles game-state))
         (game-render/aiming (frame-ufo game-state)
                             (frame-cannon game-state))]
        [else (game-render/fired (frame-ufo game-state)
                                 (frame-cannon game-state)
                                 (frame-missiles game-state))]))

;; List-of-Posns -> Boolean
;; checks if a missile has been fired:
;; - if input is an empty list, then yes;
;; - else, no.

(check-expect (aiming? '()) #t)
(check-expect
 (aiming?
  (list (make-posn 50 MISSILES-Y-INIT)))
 #f)
(check-expect
 (aiming?
  (list (make-posn 29 MISSILES-Y-INIT)
        (make-posn 100 158)
        (make-posn 122 95)))
 #f)

(define (aiming? missiles)
  (empty? missiles))

;; Posn Number -> Image
;; renders the following images onto SCENE:
;; - UFO
;; - CANNON.

(check-expect
 (game-render/aiming (frame-ufo FRAME-INIT) (frame-cannon FRAME-INIT))
 (place-images (list UFO
                     CANNON)
               (list (make-posn (posn-x (frame-ufo FRAME-INIT))
                                (posn-y (frame-ufo FRAME-INIT)))
                     (make-posn (frame-cannon FRAME-INIT)
                                CANNON-Y))
               SCENE))

(define (game-render/aiming ufo cannon-x)
  (place-images (list UFO
                      CANNON)
                (list (make-posn (posn-x ufo) (posn-y ufo))
                      (make-posn cannon-x CANNON-Y))
                SCENE))

;; Posn Number List-of-Posns -> Image
;; renders the following images onto SCENE:
;; - UFO
;; - CANNON
;; - MISSILEs.

(check-expect
 (game-render/fired
  (make-posn 148 89)
  23
  (list (make-posn 29 MISSILES-Y-INIT)
        (make-posn 100 158)
        (make-posn 122 95)))
 (place-images (list UFO
                     CANNON)
               (list (make-posn 148 89)
                     (make-posn 23 CANNON-Y))
               (missiles-render (list (make-posn 29 MISSILES-Y-INIT)
                                      (make-posn 100 158)
                                      (make-posn 122 95)))))

(define (game-render/fired ufo cannon-x missiles)
  (place-images (list UFO
                      CANNON)
                (list (make-posn (posn-x ufo) (posn-y ufo))
                      (make-posn cannon-x CANNON-Y))
                (missiles-render missiles)))

;; List-of-Posns -> Image
;; renders the image of each missile fired.

(check-expect
 (missiles-render
  (list (make-posn 50 MISSILES-Y-INIT)))
 (place-image MISSILE 50 MISSILES-Y-INIT SCENE))
(check-expect
 (missiles-render
  (list (make-posn 29 MISSILES-Y-INIT)
        (make-posn 100 158)
        (make-posn 122 95)))
 (place-image MISSILE 29 MISSILES-Y-INIT
              (missiles-render (list (make-posn 100 158)
                                     (make-posn 122 95)))))

(define (missiles-render missiles)
  (cond [(empty? missiles) SCENE]
        [else (place-image MISSILE
                           (posn-x (car missiles))
                           (posn-y (car missiles))
                           (missiles-render (cdr missiles)))]))

;; Game-State -> Game-State
;; moves the following per clock tick:
;; - UFO
;; - missiles.

(check-random
 (game-move FRAME-INIT)
 (frame (ufo-move (frame-ufo FRAME-INIT) (frame-missiles FRAME-INIT))
        (frame-cannon FRAME-INIT)
        (missiles-move (frame-missiles FRAME-INIT))))
(check-random
 (game-move
  (frame (make-posn 148 89)
         23
         (list (make-posn 29 MISSILES-Y-INIT)
               (make-posn 100 158)
               (make-posn 122 95))))
  (frame (ufo-move (make-posn 148 89)
                   (list (make-posn 29 MISSILES-Y-INIT)
                         (make-posn 100 158)
                         (make-posn 122 95)))
         23
         (missiles-move (list (make-posn 29 MISSILES-Y-INIT)
                              (make-posn 100 158)
                              (make-posn 122 95)))))

(define (game-move game-state)
  (frame (ufo-move (frame-ufo game-state) (frame-missiles game-state))
         (frame-cannon game-state)
         (missiles-move (frame-missiles game-state))))

;; Posn List-of-Posns -> Posn
;; determines the Posn of the UFO per clock tick.

(check-random
 (ufo-move (frame-ufo FRAME-INIT) (frame-missiles FRAME-INIT))
 (make-posn (ufo-move/x (ufo-move/warp (posn-x UFO-POSN-INIT)))
            (ufo-move/y UFO-POSN-INIT (frame-missiles FRAME-INIT))))

(define (ufo-move ufo missiles)
  (make-posn (ufo-move/x (ufo-move/warp (posn-x ufo)))
             (ufo-move/y ufo missiles)))

;; Number -> Number
;; randomizes the UFO's x-coordinate by half of its
;; width per clock tick.

(check-random
 (ufo-move/warp (posn-x UFO-POSN-INIT))
 ((if (= (random 2) 1) + -)
  (posn-x UFO-POSN-INIT) (random UFO-WARP-RANGE-MIN UFO-WARP-RANGE-MAX)))

(define (ufo-move/warp ufo-x)
  ((if (= (random 2) 1) + -)
   ufo-x (random UFO-WARP-RANGE-MIN UFO-WARP-RANGE-MAX)))

;; Number -> Number
;; determines the x-coordinate of the UFO per clock tick.

(check-random (ufo-move/x (ufo-move/warp 0)) (+ (ufo-move/warp 0) UFO-ADJUSTMENT-X))
(check-random (ufo-move/x (ufo-move/warp SCENE-WIDTH)) (- (ufo-move/warp SCENE-WIDTH) UFO-ADJUSTMENT-X))
(check-random (ufo-move/x (ufo-move/warp (posn-x UFO-POSN-INIT))) (ufo-move/warp (posn-x UFO-POSN-INIT)))

(define (ufo-move/x ufo-x-warped)
  (cond [(<= ufo-x-warped UFO-LIMIT-LEFT) (+ ufo-x-warped UFO-ADJUSTMENT-X)]
        [(>= ufo-x-warped UFO-LIMIT-RIGHT) (- ufo-x-warped UFO-ADJUSTMENT-X)]
        [else ufo-x-warped]))

;; Posn List-of-Posns -> Number
;; does the following:
;; - if a missile hits the UFO, the UFO is moved upward by UFO-VELOCITY-RETREATING
;; - else, moves the UFO by UFO-VELOCITY-LANDING from the top of SCENE per clock tick.

(check-expect (ufo-move/y UFO-POSN-INIT (frame-missiles FRAME-INIT)) (+ (posn-y UFO-POSN-INIT) UFO-VELOCITY-LANDING))
(check-expect
 (ufo-move/y
  (make-posn 148 89)
  (list (make-posn 29 MISSILES-Y-INIT)
        (make-posn 100 158)
        (make-posn 122 95)))
 (- 89 UFO-VELOCITY-RETREATING))

(define (ufo-move/y ufo missiles)
  (cond [(ufo-hit? ufo missiles) (- (posn-y ufo) UFO-VELOCITY-RETREATING)]
        [else (+ (posn-y ufo) UFO-VELOCITY-LANDING)]))

;; Posn List-of-Posns -> Boolean
;; checks if a missile has hit the UFO.

(check-expect (ufo-hit? (frame-ufo FRAME-INIT) (frame-missiles FRAME-INIT)) #f)
(check-expect
 (ufo-hit?
  (make-posn 148 89)
  (list (make-posn 29 MISSILES-Y-INIT)
        (make-posn 100 158)
        (make-posn 122 95)))
 #t)

(define (ufo-hit? ufo missiles)
  (cond [(empty? missiles) #f]
        [else (or (within-hit-range? ufo (car missiles))
                  (ufo-hit? ufo (cdr missiles)))]))

;; Posn Posn -> Boolean
;; checks if a missile is touching the UFO.

(check-expect (within-hit-range? (make-posn 148 89) (make-posn 29 MISSILES-Y-INIT)) #f)
(check-expect (within-hit-range? (make-posn 148 89) (make-posn 100 158)) #f)
(check-expect (within-hit-range? (make-posn 148 89) (make-posn 122 95)) #t)

(define (within-hit-range? ufo missile)
  (<=
   (sqrt (+ (sqr (- (posn-x ufo) (posn-x missile)))
            (sqr (- (posn-y ufo) (posn-y missile)))))
   UFO-HIT-RANGE))

;; Posn -> Posn
;; determines the Posn of MISSILES per clock tick.

(check-expect (missiles-move (frame-missiles FRAME-INIT)) '())
(check-expect
 (missiles-move
  (list (make-posn 29 MISSILES-Y-INIT)
        (make-posn 100 158)
        (make-posn 122 95)))
 (cons (make-posn 29 (- MISSILES-Y-INIT MISSILE-VELOCITY))
       (missiles-move (list (make-posn 100 158)
                            (make-posn 122 95)))))

(define (missiles-move missiles)
  (cond [(aiming? missiles) '()]
        [else (cons (make-posn (posn-x (car missiles))
                               (- (posn-y (car missiles)) MISSILE-VELOCITY))
                    (missiles-move (cdr missiles)))]))

;; Game-State Key-Event -> Game-State
;; does the following according to Key-Event:
;; - "left" or "left": moves the cannon left or right on SCENE
;; - "space": launches a missile

(check-expect
 (game-control FRAME-INIT "left")
 (frame (frame-ufo FRAME-INIT)
        (cannon-move (frame-cannon FRAME-INIT) "left")
        (missiles-launch (frame-cannon FRAME-INIT) (frame-missiles FRAME-INIT) "left")))
(check-expect
 (game-control FRAME-INIT "right")
 (frame (frame-ufo FRAME-INIT)
        (cannon-move (frame-cannon FRAME-INIT) "right")
        (missiles-launch (frame-cannon FRAME-INIT) (frame-missiles FRAME-INIT) "right")))
(check-expect
 (game-control FRAME-INIT " ")
 (frame (frame-ufo FRAME-INIT)
        (cannon-move (frame-cannon FRAME-INIT) " ")
        (missiles-launch (frame-cannon FRAME-INIT) (frame-missiles FRAME-INIT) " ")))
(check-expect
 (game-control FRAME-INIT "p")
 (frame (frame-ufo FRAME-INIT)
        (cannon-move (frame-cannon FRAME-INIT) "p")
        (missiles-launch (frame-cannon FRAME-INIT) (frame-missiles FRAME-INIT) "p")))

(define (game-control game-state key-event)
  (frame (frame-ufo game-state)
         (cannon-move (frame-cannon game-state) key-event)
         (missiles-launch (frame-cannon game-state) (frame-missiles game-state) key-event)))

;; Number Key-Event -> Number
;; moves the cannon left of right according to Key-Event.

(check-expect (cannon-move CANNON-X-INIT "left") (- CANNON-X-INIT CANNON-VELOCITY))
(check-expect (cannon-move (- CANNON-LIMIT-LEFT 1) "left") (- CANNON-LIMIT-LEFT 1))
(check-expect (cannon-move CANNON-X-INIT "right") (+ CANNON-X-INIT CANNON-VELOCITY))
(check-expect (cannon-move (+ CANNON-LIMIT-RIGHT 1) "right") (+ CANNON-LIMIT-RIGHT 1))
(check-expect (cannon-move CANNON-X-INIT "p") CANNON-X-INIT)

(define (cannon-move cannon key-event)
  (cond [(and (key=? key-event "left") (>= cannon CANNON-LIMIT-LEFT)) (- cannon CANNON-VELOCITY)]
        [(and (key=? key-event "right") (<= cannon CANNON-LIMIT-RIGHT)) (+ cannon CANNON-VELOCITY)]
        [else cannon]))

;; Number List-of-Posns Key-Event -> Posn
;; launches a missile at the cannon's Posn when the user inputs " ".

(check-expect
 (missiles-launch (frame-cannon FRAME-INIT) (frame-missiles FRAME-INIT) "o")
 (frame-missiles FRAME-INIT))
(check-expect
 (missiles-launch (frame-cannon FRAME-INIT) (frame-missiles FRAME-INIT) " ")
 (cons (make-posn (frame-cannon FRAME-INIT) MISSILES-Y-INIT) (frame-missiles FRAME-INIT)))

(define (missiles-launch cannon missiles key-event)
  (cond [(not (key=? key-event " ")) missiles]
        [else (cons (make-posn cannon MISSILES-Y-INIT) missiles)]))

;; Game-State -> Boolean
;; checks either of the following:
;; - the UFO has retreated
;; - the UFO has landed.

(check-expect (game-over? FRAME-INIT) #f)
(check-expect
 (game-over?
  (frame (make-posn 89 -50)
         56
         (list (make-posn 29 MISSILES-Y-INIT)
               (make-posn 100 158)
               (make-posn 122 95))))
 #t)
(check-expect
 (game-over?
  (frame (make-posn 89 UFO-LANDED)
         56
         (list (make-posn 29 MISSILES-Y-INIT)
               (make-posn 100 158)
               (make-posn 122 95))))
 #t)

(define (game-over? game-state)
  (or (<= (posn-y (frame-ufo game-state)) UFO-RETREATED) (>= (posn-y (frame-ufo game-state)) UFO-LANDED)))

;; Game-State -> Image
;; renders the game-over screen.

(check-expect
 (game-over-render
  (frame (make-posn 89 -50)
         56
         (list (make-posn 29 MISSILES-Y-INIT)
               (make-posn 100 158)
               (make-posn 122 95))))
 (overlay (text "how could you." 25 "black")
          (game-render
           (frame (make-posn 89 -50)
                  56
                  (list (make-posn 29 MISSILES-Y-INIT)
                        (make-posn 100 158)
                        (make-posn 122 95))))))
(check-expect
 (game-over-render
  (frame (make-posn 89 UFO-LANDED)
         56
         (list (make-posn 29 MISSILES-Y-INIT)
               (make-posn 100 158)
               (make-posn 122 95))))
 (overlay (text "they're here." 25 "black")
          (game-render
           (frame (make-posn 89 UFO-LANDED)
                  56
                  (list (make-posn 29 MISSILES-Y-INIT)
                        (make-posn 100 158)
                        (make-posn 122 95))))))

(define (game-over-render game-state)
  (overlay (text (cond [(<= (posn-y (frame-ufo game-state)) UFO-RETREATED) "how could you."]
                       [else "they're here."])
                 25 "black")
           (game-render game-state)))

;;;; application

(test)
;; (ufo-main FRAME-INIT)
