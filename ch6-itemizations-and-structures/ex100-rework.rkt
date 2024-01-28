#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; constant and data definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 400)
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT "maroon"))

(define UFO-WIDTH (/ CANVAS-WIDTH 6))
(define UFO
  (overlay (circle (/ UFO-WIDTH 4) "solid" "navy")
           (rectangle UFO-WIDTH (/ CANVAS-HEIGHT 80) "solid" "navy")))
(define UFO-HIT
  (underlay UFO
            (text/font "deads" 10 "yellow"
                       #f "script" "normal" "bold" #f)))
(define UFO-VELOCITY 2)
(define UFO-INIT-X (/ CANVAS-WIDTH 2))
(define UFO-INIT-Y (/ (image-height UFO) 2))
(define UFO-X-DELTA 2)
(define UFO-X-LIMIT-LEFT (/ UFO-WIDTH 2))
(define UFO-X-LIMIT-RIGHT (- CANVAS-WIDTH (/ UFO-WIDTH 2)))
(define UFO-WARP 4)

(define EARTH (- CANVAS-HEIGHT (/ (image-height UFO) 2)))

(define TANK-WIDTH (/ CANVAS-WIDTH 8))
(define TANK-HEIGHT (/ CANVAS-HEIGHT 20))
(define TANK (rectangle TANK-WIDTH TANK-HEIGHT "solid" "black"))
(define TANK-VELOCITY-RIGHT 4)
(define TANK-VELOCITY-LEFT -4)
(define TANK-INIT-X (/ TANK-WIDTH 2))
(define TANK-Y (- CANVAS-HEIGHT (/ TANK-HEIGHT 2)))
(define TANK-X-LIMIT-LEFT (/ TANK-WIDTH 2))
(define TANK-X-LIMIT-RIGHT (- CANVAS-WIDTH (/ TANK-WIDTH 2)))

(define MISSILE-SIDE-LENGTH (/ TANK-WIDTH 3))
(define MISSILE (isosceles-triangle MISSILE-SIDE-LENGTH 30 "solid" "yellow"))
(define MISSILE-VELOCITY (* TANK-VELOCITY-RIGHT 4))
(define MISSILE-INIT-Y (- CANVAS-HEIGHT (image-height MISSILE)))

(define USER-WON (text "you win." 20 "black"))
(define USER-LOST (text "you lose." 20 "black"))

;; a UFO-Coordinate is a Posn.
;; (make-posn x y) is the UFO's location
;; (using the topdown, left-to-right convention).

;; a Tank-Coordinate is a structure:
;;  (make-tank Number Number).
;; (make-tank x dx) specifies the position:
;; (x, TANK-Y) and the tank's speed: dx pixels/tick.
(define-struct tank [x dx])

;; a Missile-Coordinate is a Posn.
;; (make-posn x y) is the missiles location.

;; an Aim is a structure:
;;  (make-aim UFO-Coordinate Tank-Coordinate)
;; (make-aim ufo-cor tank-cor) represents the
;; locations of UFO and TANK before MISSILE is fired.
(define-struct aim [ufo-cor tank-cor])

;; a Fired is a structure:
;;  (make-fired UFO-Coordinate Tank-Coordinate Missile-Coordinate)
;; (make-fired  ufo-cor tank-cor missile-cor)
;; represents the locations of UFO, TANK, and MISSILE
;; after MISSILE is fired.
(define-struct fired [ufo-cor tank-cor missile-cor])

;; a SIGS is one of:
;; - (make-aim UFO-Coordinate Tank-Coordinate)
;; - (make-fired UFO-Coordinate Tank-Coordinate Missile-Coordinate)
;; represetnts the complete state of a Space Invader game.
(define sigs-ex0
  (make-aim (make-posn 20 10)
            (make-tank 28 (- TANK-VELOCITY-RIGHT))))
(define sigs-ex1
  (make-fired (make-posn 20 10)
              (make-tank 28 (- TANK-VELOCITY-RIGHT))
              (make-posn 28 (- CANVAS-HEIGHT TANK-HEIGHT))))
(define sigs-ex2
  (make-fired (make-posn 20 100)
              (make-tank 100 TANK-VELOCITY-RIGHT)
              (make-posn 22 103)))
(define sigs-ex3
  (make-aim (make-posn 20 EARTH)
            (make-tank 100 3)))
(define sigs-ex4
  (make-fired (make-posn 20 EARTH)
              (make-tank 100 3)
              (make-posn 150 45)))

(define GAME-INIT-STATE
  (make-aim (make-posn UFO-INIT-X UFO-INIT-Y)
            (make-tank TANK-INIT-X TANK-VELOCITY-RIGHT)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SIGS -> Image
;; renders a SIGS into an image onto CANVAS according
;; to the SIGS data given:
;; - Aim
;; - Fired
(check-expect (si-render sigs-ex0)
              (tank-render (aim-tank-cor sigs-ex0)
                           (ufo-render (aim-ufo-cor sigs-ex0) CANVAS)))
(check-expect (si-render sigs-ex1)
              (tank-render (fired-tank-cor sigs-ex1)
                           (ufo-render (fired-ufo-cor sigs-ex1)
                                       (missile-render (fired-missile-cor sigs-ex1)
                                                       CANVAS))))
(check-expect (si-render sigs-ex2)
              (tank-render (fired-tank-cor sigs-ex2)
                           (ufo-render (fired-ufo-cor sigs-ex2)
                                       (missile-render (fired-missile-cor sigs-ex2)
                                                       CANVAS))))

(define (si-render s)
  (cond [(aim? s) (tank-render (aim-tank-cor s)
                               (ufo-render (aim-ufo-cor s) CANVAS))]
        [(fired? s) (tank-render (fired-tank-cor s)
                                 (ufo-render (fired-ufo-cor s)
                                             (missile-render (fired-missile-cor s)
                                                             CANVAS)))]))

;; SIGS -> Image
;; renders final state of the program into an image
;; onto CANVAS
(check-expect (si-render-final sigs-ex3)
              (overlay USER-LOST
                       (tank-render (aim-tank-cor sigs-ex3)
                                    (ufo-render (aim-ufo-cor sigs-ex3) CANVAS))))
(check-expect (si-render-final sigs-ex2)
              (overlay USER-WON
                       (tank-render (fired-tank-cor sigs-ex2)
                                    (ufo-render (fired-ufo-cor sigs-ex2)
                                                (missile-render (fired-missile-cor sigs-ex2)
                                                                CANVAS)))))
(check-expect (si-render-final sigs-ex4)
              (overlay USER-LOST
                       (tank-render (fired-tank-cor sigs-ex4)
                                    (ufo-render (fired-ufo-cor sigs-ex4)
                                                (missile-render (fired-missile-cor sigs-ex4)
                                                                CANVAS)))))

(define (si-render-final s)
  (cond [(aim? s) (overlay USER-LOST
                           (tank-render (aim-tank-cor s)
                                        (ufo-render (aim-ufo-cor s) CANVAS)))]
        [(fired? s) (if (>= (posn-y (fired-ufo-cor s)) EARTH)
                        (overlay USER-LOST
                                 (tank-render (fired-tank-cor s)
                                              (ufo-render (fired-ufo-cor s)
                                                          (missile-render (fired-missile-cor s)
                                                                          CANVAS))))
                        (overlay USER-WON
                                 (tank-render (fired-tank-cor s)
                                              (ufo-render (fired-ufo-cor s)
                                                          (missile-render (fired-missile-cor s)
                                                                          CANVAS)))))]))

;; Tank-Coordinate Image -> Image
;; renders a Tank-Coordinate into an image onto given Image.
(check-expect (tank-render (aim-tank-cor sigs-ex0)
                           (ufo-render (aim-ufo-cor sigs-ex0) CANVAS))
              (place-image TANK (tank-x (aim-tank-cor sigs-ex0)) TANK-Y
                           (ufo-render (aim-ufo-cor sigs-ex0) CANVAS)))

(define (tank-render tc im)
  (place-image TANK (tank-x tc) TANK-Y im))

;; UFO-Coordinate Image -> Image
;; renders a UFO-Coordinate into an image onto given Image.
(check-expect (ufo-render (aim-ufo-cor sigs-ex0) CANVAS)
              (place-image UFO
                           (posn-x (aim-ufo-cor sigs-ex0))
                           (posn-y (aim-ufo-cor sigs-ex0))
                           CANVAS))
(check-expect (ufo-render (fired-ufo-cor sigs-ex1)
                          (missile-render (fired-missile-cor sigs-ex1)
                                          CANVAS))
              (place-image UFO
                           (posn-x (fired-ufo-cor sigs-ex1))
                           (posn-y (fired-ufo-cor sigs-ex1))
                           (missile-render (fired-missile-cor sigs-ex1)
                                           CANVAS)))

(define (ufo-render uc im)
  (place-image UFO (posn-x uc) (posn-y uc) im))

;; Missile-Coordinate Image -> Image
;; renders a Missile-Coordinate into an image onto given Image.
(check-expect (missile-render (fired-missile-cor sigs-ex2) CANVAS)
              (place-image MISSILE
                           (posn-x (fired-missile-cor sigs-ex2))
                           (posn-y (fired-missile-cor sigs-ex2))
                           CANVAS))

(define (missile-render mc im)
  (place-image MISSILE (posn-x mc) (posn-y mc) im))

;; SIGS -> SIGS
;; determines the position UFO, TANK, and MISSILE (if any)
;; per clock tick.
(check-random (si-move sigs-ex0) (si-move-proper sigs-ex0 (random UFO-WARP)))

(define (si-move s)
  (si-move-proper s (random UFO-WARP)))

;; SIGS Number -> SIGS
;; moves the space-invader objects predictably by delta.
(check-expect (si-move-proper sigs-ex0 4)
              (make-aim (ufo-move (aim-ufo-cor sigs-ex0) 4)
                        (tank-move (aim-tank-cor sigs-ex0))))
(check-expect (si-move-proper sigs-ex1 2)
              (make-fired (ufo-move (fired-ufo-cor sigs-ex1) 2)
                          (tank-move (fired-tank-cor sigs-ex1))
                          (missile-move (fired-missile-cor sigs-ex1))))

(define (si-move-proper s delta)
  (cond [(aim? s) (make-aim (ufo-move (aim-ufo-cor s) delta)
                            (tank-move (aim-tank-cor s)))]
        [(fired? s) (make-fired (ufo-move (fired-ufo-cor s) delta)
                                (tank-move (fired-tank-cor s))
                                (missile-move (fired-missile-cor s)))]))

;; UFO-Coordinate Number -> UFO-Coordinate
;; calculates the next UFO-Coordinate depending on si-move.
(check-expect (ufo-move (aim-ufo-cor sigs-ex0) 1)
              (make-posn (push-ufo-within-frame (+ (posn-x (aim-ufo-cor sigs-ex0)) (ufo-jumps-by 1)))
                         (+ (posn-y (aim-ufo-cor sigs-ex0)) UFO-VELOCITY)))

(define (ufo-move uc delta)
  (make-posn (push-ufo-within-frame (+ (posn-x uc) (ufo-jumps-by delta)))
             (+ (posn-y uc) UFO-VELOCITY)))

;; Number -> Number
;; adjusts x-coordinate of UFO if it jumps out of frame
(check-expect (push-ufo-within-frame CANVAS-WIDTH) (- CANVAS-WIDTH UFO-WARP))
(check-expect (push-ufo-within-frame 0) (+ 0 UFO-WARP))
(check-expect (push-ufo-within-frame (/ CANVAS-WIDTH 2)) (/ CANVAS-WIDTH 2))

(define (push-ufo-within-frame ufo-new-x)
  (cond [(<= ufo-new-x UFO-X-LIMIT-LEFT) (+ ufo-new-x UFO-WARP)]
        [(>= ufo-new-x UFO-X-LIMIT-RIGHT) (- ufo-new-x UFO-WARP)]
        [else ufo-new-x]))

;; Number -> Number
;; calculates the number of pixels UFO jumps by.
(check-expect (ufo-jumps-by 2) 2)
(check-expect (ufo-jumps-by 1) -1)

(define (ufo-jumps-by delta)
  (if (= (modulo delta 2) 0)
      delta
      (- delta)))

;; Tank-Coordinate -> Tank-Coordinate
;; calculates the next Tank-Coordinate depending on si-move.
(check-expect (tank-move (aim-tank-cor sigs-ex0))
              (make-tank (+ (tank-x (aim-tank-cor sigs-ex0)) (tank-dx (aim-tank-cor sigs-ex0)))
                         (tank-dx (aim-tank-cor sigs-ex0))))

(define (tank-move tc)
  (make-tank (+ (tank-x tc) (tank-dx tc))
             (tank-direction (tank-x tc) (tank-dx tc))))

;; Number -> Number
;; changes TANK's direction when it is about to go out of frame.
(check-expect (tank-direction CANVAS-WIDTH TANK-VELOCITY-RIGHT) TANK-VELOCITY-LEFT)
(check-expect (tank-direction 0 TANK-VELOCITY-LEFT) TANK-VELOCITY-RIGHT)
(check-expect (tank-direction (/ CANVAS-WIDTH 2) TANK-VELOCITY-RIGHT) TANK-VELOCITY-RIGHT)

(define (tank-direction tx tdx)
  (cond [(> tx TANK-X-LIMIT-RIGHT) TANK-VELOCITY-LEFT]
        [(< tx TANK-X-LIMIT-LEFT) TANK-VELOCITY-RIGHT]
        [else tdx]))

;; Missile-Coordinate -> Missile-Coordinate
;; calculates the next Missile-Coordinate depending on si-move.
(check-expect (missile-move (fired-missile-cor sigs-ex2))
              (make-posn (posn-x (fired-missile-cor sigs-ex2))
                         (- (posn-y (fired-missile-cor sigs-ex2)) MISSILE-VELOCITY)))

(define (missile-move mc)
  (make-posn (posn-x mc)
             (- (posn-y mc) MISSILE-VELOCITY)))

;; SIGS KeyEvent -> SIGS
;; changes direction of TANK and launches MISSILE
;; depending on user's keyboard input:
;; - "left" | makes TANK go left
;; - "right" | makes TANK go right
;; - " " | fires MISSILE if not yet fired
(check-expect (si-control sigs-ex0 "left")
              (make-aim (make-posn (posn-x (aim-ufo-cor sigs-ex0))
                                   (posn-y (aim-ufo-cor sigs-ex0)))
                        (make-tank (tank-x (aim-tank-cor sigs-ex0))
                                   (tank-change-direction (tank-dx (aim-tank-cor sigs-ex0))
                                                          "left"))))
(check-expect (si-control sigs-ex0 "right")
              (make-aim (make-posn (posn-x (aim-ufo-cor sigs-ex0))
                                   (posn-y (aim-ufo-cor sigs-ex0)))
                        (make-tank (tank-x (aim-tank-cor sigs-ex0))
                                   (tank-change-direction (tank-dx (aim-tank-cor sigs-ex0))
                                                          "right"))))
(check-expect (si-control sigs-ex0 " ")
              (make-fired (make-posn (posn-x (aim-ufo-cor sigs-ex0))
                                     (posn-y (aim-ufo-cor sigs-ex0)))
                          (make-tank (tank-x (aim-tank-cor sigs-ex0))
                                     (tank-dx (aim-tank-cor sigs-ex0)))
                          (make-posn (tank-x (aim-tank-cor sigs-ex0))
                                     MISSILE-INIT-Y)))
(check-expect (si-control sigs-ex0 "j") sigs-ex0)
(check-expect (si-control sigs-ex1 "left")
              (make-fired (make-posn (posn-x (fired-ufo-cor sigs-ex1))
                                     (posn-y (fired-ufo-cor sigs-ex1)))
                          (make-tank (tank-x (fired-tank-cor sigs-ex1))
                                     (tank-change-direction (tank-dx (fired-tank-cor sigs-ex1))
                                                            "left"))
                          (make-posn (posn-x (fired-missile-cor sigs-ex1))
                                     (posn-y (fired-missile-cor sigs-ex1)))))
(check-expect (si-control sigs-ex1 "right")
              (make-fired (make-posn (posn-x (fired-ufo-cor sigs-ex1))
                                     (posn-y (fired-ufo-cor sigs-ex1)))
                          (make-tank (tank-x (fired-tank-cor sigs-ex1))
                                     (tank-change-direction (tank-dx (fired-tank-cor sigs-ex1))
                                                            "right"))
                          (make-posn (posn-x (fired-missile-cor sigs-ex1))
                                     (posn-y (fired-missile-cor sigs-ex1)))))
(check-expect (si-control sigs-ex1 " ") sigs-ex1)
(check-expect (si-control sigs-ex1 "k") sigs-ex1)

(define (si-control s ke)
  (cond [(or (key=? ke "left") (key=? ke "right"))
         (if (aim? s)
             (make-aim (make-posn (posn-x (aim-ufo-cor s))
                                  (posn-y (aim-ufo-cor s)))
                       (make-tank (tank-x (aim-tank-cor s))
                                  (tank-change-direction (tank-dx (aim-tank-cor s)) ke)))
             (make-fired (make-posn (posn-x (fired-ufo-cor s))
                                    (posn-y (fired-ufo-cor s)))
                         (make-tank (tank-x (fired-tank-cor s))
                                    (tank-change-direction (tank-dx (fired-tank-cor s)) ke))
                         (make-posn (posn-x (fired-missile-cor s))
                                    (posn-y (fired-missile-cor s)))))]
        [(key=? ke " ")
         (if (aim? s)
             (make-fired (make-posn (posn-x (aim-ufo-cor s))
                                    (posn-y (aim-ufo-cor s)))
                         (make-tank (tank-x (aim-tank-cor s))
                                    (tank-dx (aim-tank-cor s)))
                         (make-posn (tank-x (aim-tank-cor s))
                                    MISSILE-INIT-Y))
             s)]
        [else s]))

;; Number KeyEvent -> Number
;; changes the direction of TANK depending on keyboard input:
;; - "left" | makes TANK go left
;; - "right" | makes TANK go right
(check-expect (tank-change-direction TANK-VELOCITY-LEFT "right") TANK-VELOCITY-RIGHT)
(check-expect (tank-change-direction TANK-VELOCITY-RIGHT "left") TANK-VELOCITY-LEFT)
(check-expect (tank-change-direction TANK-VELOCITY-LEFT "g") TANK-VELOCITY-LEFT)

(define (tank-change-direction tdx ke)
  (cond [(key=? ke "left") TANK-VELOCITY-LEFT]
        [(key=? ke "right") TANK-VELOCITY-RIGHT]
        [else tdx]))


;; FIRED -> Boolean
;; stops the game if any of the following is true:
;; - UFO lands
;; - MISSILE hits UFO
(check-expect (ufo-hit? (fired-ufo-cor sigs-ex2) (fired-missile-cor sigs-ex2)) #t)
(check-expect (ufo-hit? (fired-ufo-cor (make-fired (make-posn 20 100)
                                                   (make-tank 100 TANK-VELOCITY-RIGHT)
                                                   (make-posn 40 143)))
                        (fired-missile-cor (make-fired (make-posn 20 100)
                                                       (make-tank 100 TANK-VELOCITY-RIGHT)
                                                       (make-posn 40 143)))) #f)

(define (game-over? s)
  (cond [(aim? s) (>= (posn-y (aim-ufo-cor s)) EARTH)]
        [(fired? s) (or (>= (posn-y (fired-ufo-cor s)) EARTH)
                        (ufo-hit? (fired-ufo-cor s) (fired-missile-cor s)))]
        [else #f]))

;; UFO-Coordinate Missile-Coordinate -> Boolean
;; checks if the tip of MISSILE has hit UFO. conditions
;; for hit:
;; - x-coordinate of MISSILE falls within width of UFO
;; - tip of MISSILE falls within height of UFO
(check-expect (ufo-hit? (fired-ufo-cor sigs-ex2) (fired-missile-cor sigs-ex2)) #t)
(check-expect (ufo-hit? (fired-ufo-cor (make-fired (make-posn 20 100)
                                                   (make-tank 100 TANK-VELOCITY-RIGHT)
                                                   (make-posn 40 143)))
                        (fired-missile-cor (make-fired (make-posn 20 100)
                                                       (make-tank 100 TANK-VELOCITY-RIGHT)
                                                       (make-posn 40 143)))) #f)

(define (ufo-hit? uc mc)
  (and (<= (- (posn-x uc) (/ (image-width UFO) 2))
           (posn-x mc)
           (+ (posn-x uc) (/ (image-width UFO) 2)))
       (<= (- (posn-y uc) (/ (image-height UFO) 2))
           (- (posn-y mc) (/ (image-height MISSILE) 2))
           (+ (posn-y uc) (/ (image-height UFO) 2)))))

;; Any -> SIGS
;; is s an example of the SIGS collection
(check-expect (game-state? sigs-ex0) #t)
(check-expect (game-state? "potato") #f)
(check-expect (game-state? empty-image) #f)
(check-expect (game-state? #f) #f)
(check-expect (game-state? 23) #f)

(define (game-state? s)
  (or (aim? s) (fired? s)))

;; SIGS -> SIGS
;; main function.
(define (si-main s)
  (big-bang s
            [to-draw si-render]
            [on-tick si-move]
            [on-key si-control]
            [stop-when game-over? si-render-final]
            [check-with game-state?]))

(si-main GAME-INIT-STATE)
(test)
