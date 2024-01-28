#lang racket
(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; constant and data definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 200)
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT "maroon"))

(define UFO-WIDTH (/ CANVAS-WIDTH 6))
(define UFO
  (overlay (circle (/ UFO-WIDTH 4) "solid" "navy")
           (rectangle UFO-WIDTH (/ CANVAS-HEIGHT 80) "solid" "navy")))
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
(define MISSILE-INIT-Y (- CANVAS-HEIGHT (image-height MISSILE) 10))

(define HIT-DISTANCE 20)

(define USER-WON (text "you win." 20 "black"))
(define USER-LOST (text "you lose." 20 "black"))

;; a UFOCoordinate is a Posn.
;; (make-posn x y) is the UFO's location
;; (using the topdown, left-to-right convention).

;; a TankCoordinate is a structure:
;;  (make-tank Number Number).
;; (make-tank x dx) specifies the position:
;; (x, TANK-Y) and the tank's speed: dx pixels/tick.
(define-struct tank [x dx])

;; a MissileOrNot is one of:
;; - #f
;; - Posn
;; #f means the missile is in the tank; Posn says
;; the missile is at that location.

;; a SIGS is:
;;  (make-sigs UFOCoordinate TankCoordinate MissileOrNot)
;; represents the complete state of a Space Invader game.
(define-struct sigs [ufo tank missile/#f])

(define sigs-ex0
  (make-sigs (make-posn 20 10)
             (make-tank 28 TANK-VELOCITY-RIGHT)
             #f))
(define sigs-ex1
  (make-sigs (make-posn 20 10)
             (make-tank 32 TANK-VELOCITY-RIGHT)
             (make-posn 32 MISSILE-INIT-Y)))
(define sigs-ex2
  (make-sigs (make-posn 20 100)
             (make-tank 100 TANK-VELOCITY-RIGHT)
             (make-posn 22 103)))
(define sigs-ex3
  (make-sigs (make-posn 20 EARTH)
             (make-tank 100 3)
             (make-posn 150 45)))


(define GAME-INIT-STATE
  (make-sigs (make-posn UFO-INIT-X UFO-INIT-Y)
             (make-tank TANK-INIT-X TANK-VELOCITY-RIGHT)
             #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; function definitions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SIGS -> image
;; renders the state of the game onto CANVAS.
(check-expect (si-render sigs-ex0)
              (place-images
               (list UFO TANK MISSILE)
               (list (sigs-ufo sigs-ex0)
                     (make-posn (tank-x (sigs-tank sigs-ex0)) TANK-Y)
                     (make-posn (tank-x (sigs-tank sigs-ex0)) TANK-Y))
               CANVAS))
(check-expect (si-render sigs-ex1)
              (place-images
               (list UFO TANK MISSILE)
               (list (sigs-ufo sigs-ex1)
                     (make-posn (tank-x (sigs-tank sigs-ex1)) TANK-Y)
                     (sigs-missile/#f sigs-ex1))
               CANVAS))

(define (si-render s)
  (place-images
   (list UFO TANK MISSILE)
   (list (sigs-ufo s)
         (make-posn (tank-x (sigs-tank s)) TANK-Y)
         (missile-placement s))
   CANVAS))

;; SIGS -> Posn
;; determines coordinates of MISSILE. if:
;; - MissileOrNot is #f -- MISSILE is not launched and stays hidden behind TANK.
;; - MissileOrNot is a Posn -- MISSILE is launched and becomes visible.
(check-expect (missile-placement sigs-ex0)
              (make-posn
               (tank-x (sigs-tank sigs-ex0))
               TANK-Y))
(check-expect (missile-placement sigs-ex1)
              (sigs-missile/#f sigs-ex1))

(define (missile-placement s)
  (cond
    [(boolean? (sigs-missile/#f s))
     (make-posn (tank-x (sigs-tank s)) TANK-Y)]
    [(posn? (sigs-missile/#f s))
     (sigs-missile/#f s)]))

;; SIGS -> SIGS
;; determines the movement of UFO, TANK, and MISSILE per tick.
(check-random (si-move sigs-ex0)
              (si-move-proper sigs-ex0 (random UFO-WARP)))

(define (si-move s)
  (si-move-proper s (random UFO-WARP)))

;; SIGS -> SIGS
;; moves the Space Invader objects predictably by delta.
(check-expect (si-move-proper sigs-ex0 4)
              (make-sigs
               (ufo-location/tick (sigs-ufo sigs-ex0) 4)
               (tank-location/tick (sigs-tank sigs-ex0))
               (missile-location/tick (sigs-missile/#f sigs-ex0))))

(define (si-move-proper s delta)
  (make-sigs
   (ufo-location/tick (sigs-ufo s) delta)
   (tank-location/tick (sigs-tank s))
   (missile-location/tick (sigs-missile/#f s))))

;; UFOCoordinate -> UFOCoordinate
;; calculates UFO's location per tick.
(check-expect (ufo-location/tick (sigs-ufo sigs-ex0) 1)
              (make-posn (ufo-location-adjustment
                          (+ (posn-x (sigs-ufo sigs-ex0)) (ufo-jumps-by 1)))
                         (+ (posn-y (sigs-ufo sigs-ex0)) UFO-VELOCITY)))

(define (ufo-location/tick su delta)
  (make-posn (ufo-location-adjustment (+ (posn-x su) (ufo-jumps-by delta)))
             (+ (posn-y su) UFO-VELOCITY)))

;; Number -> Number
;; adjusts UFO's x-coordinate to always fit within CANVAS-WIDTH.
(check-expect (ufo-location-adjustment UFO-X-LIMIT-LEFT) (+ UFO-X-LIMIT-LEFT UFO-WARP))
(check-expect (ufo-location-adjustment UFO-X-LIMIT-RIGHT) (- UFO-X-LIMIT-RIGHT UFO-WARP))
(check-expect (ufo-location-adjustment (/ CANVAS-WIDTH 2)) (/ CANVAS-WIDTH 2))

(define (ufo-location-adjustment ufo-new-x)
  (cond [(<= ufo-new-x UFO-X-LIMIT-LEFT) (+ ufo-new-x UFO-WARP)]
        [(>= ufo-new-x UFO-X-LIMIT-RIGHT) (- ufo-new-x UFO-WARP)]
        [else ufo-new-x]))

;; Number -> Number
;; calculates the number of pixels by which UFO jumps.
(check-expect (ufo-jumps-by 1) -1)
(check-expect (ufo-jumps-by 2) 2)

(define (ufo-jumps-by d)
  (if (= (modulo d 2) 0)
      d
      (- d)))

;; TankCoordinate -> TankCoordinate
;; calculates TANK's location per tick.
(check-expect (tank-location/tick (sigs-tank sigs-ex0))
              (make-tank (+ (tank-x (sigs-tank sigs-ex0))
                            (tank-dx (sigs-tank sigs-ex0)))
                         (tank-u-turn (tank-x (sigs-tank sigs-ex0))
                                      (tank-dx (sigs-tank sigs-ex0)))))

(define (tank-location/tick st)
  (make-tank (+ (tank-x st) (tank-dx st))
             (tank-u-turn (tank-x st) (tank-dx st))))

;; Number -> Number
;; changes TANK's direction when it's about to go out of frame.
(check-expect (tank-u-turn CANVAS-WIDTH TANK-VELOCITY-RIGHT) TANK-VELOCITY-LEFT)
(check-expect (tank-u-turn 0 TANK-VELOCITY-LEFT) TANK-VELOCITY-RIGHT)
(check-expect (tank-u-turn (/ CANVAS-WIDTH 2) TANK-VELOCITY-RIGHT) TANK-VELOCITY-RIGHT)

(define (tank-u-turn tx tdx)
  (cond [(> tx TANK-X-LIMIT-RIGHT) TANK-VELOCITY-LEFT]
        [(< tx TANK-X-LIMIT-LEFT) TANK-VELOCITY-RIGHT]
        [else tdx]))

;; MissileOrNot -> MissileOrNot
;; calculates MISSILE'S location per tick.
(check-expect (missile-location/tick (sigs-missile/#f sigs-ex0)) #f)
(check-expect (missile-location/tick (sigs-missile/#f sigs-ex1))
              (make-posn (posn-x (sigs-missile/#f sigs-ex1))
                         (- (posn-y (sigs-missile/#f sigs-ex1))
                            MISSILE-VELOCITY)))

(define (missile-location/tick sm/#f)
  (cond [(boolean? sm/#f) sm/#f]
        [(posn? sm/#f)
         (make-posn (posn-x sm/#f)
                    (- (posn-y sm/#f)
                       MISSILE-VELOCITY))]))

;; SIGS KeyEvent -> SIGS
;; changes direction of TANK and launches MISSILE depending
;; on user's keyboard input:
;; - "left" | makes TANK go left
;; - "right" | makes TANK go right
;; - " " | fires MISSILE if not yet fired.
(check-expect (si-control sigs-ex0 "left")
              (make-sigs
               (sigs-ufo sigs-ex0)
               (make-tank (+ (tank-x (sigs-tank sigs-ex0))
                             (tank-dx (sigs-tank sigs-ex0)))
                          (tank-change-direction
                           (tank-dx (sigs-tank sigs-ex0)) "left"))
               (sigs-missile/#f sigs-ex0)))
(check-expect (si-control sigs-ex0 "right")
              (make-sigs
               (sigs-ufo sigs-ex0)
               (make-tank (+ (tank-x (sigs-tank sigs-ex0))
                             (tank-dx (sigs-tank sigs-ex0)))
                          (tank-change-direction
                           (tank-dx (sigs-tank sigs-ex0)) "right"))
               (sigs-missile/#f sigs-ex0)))
(check-expect (si-control sigs-ex0 " ")
              (make-sigs (sigs-ufo sigs-ex0)
                         (sigs-tank sigs-ex0)
                         (missile-launch (sigs-missile/#f sigs-ex0)
                                         (tank-x (sigs-tank sigs-ex0)))))
(check-expect (si-control sigs-ex0 "h") sigs-ex0)

(define (si-control s ke)
  (cond [(or (key=? ke "left") (key=? ke "right"))
         (make-sigs
          (sigs-ufo s)
          (make-tank
           (+ (tank-x (sigs-tank s)) (tank-dx (sigs-tank s)))
           (tank-change-direction (tank-dx (sigs-tank s)) ke))
          (sigs-missile/#f s))]
        [(key=? ke " ")
         (make-sigs
          (sigs-ufo s)
          (sigs-tank s)
          (missile-launch (sigs-missile/#f s) (tank-x (sigs-tank s))))]
        [else s]))

;; TankCoordinate KeyEvent -> TankCoordinate
;; changes direction of TANK.
(check-expect (tank-change-direction (sigs-tank sigs-ex0) "left") TANK-VELOCITY-LEFT)
(check-expect (tank-change-direction (sigs-tank sigs-ex1) "right") TANK-VELOCITY-RIGHT)
(check-expect (tank-change-direction (sigs-tank sigs-ex1) "j") (tank-dx (sigs-tank sigs-ex1)))

(define (tank-change-direction st ke)
  (cond [(key=? ke "left") TANK-VELOCITY-LEFT]
        [(key=? ke "right") TANK-VELOCITY-RIGHT]
        [else (tank-dx st)]))

;; MissileOrNot -> MissileOrNot
;; launches missile if not yet launched.
(check-expect (missile-launch #f 12)
              (make-posn 12 MISSILE-INIT-Y))
(check-expect (missile-launch (make-posn 14 41) 12)
              (make-posn 14 41))

(define (missile-launch sm/#f tx)
  (if (boolean? sm/#f)
      (make-posn tx MISSILE-INIT-Y)
      sm/#f))

;; SIGS -> Boolean
;; determines whether the game needs to be stopped.
(check-expect (game-over? sigs-ex0) #f)
(check-expect (game-over? sigs-ex2) #t)
(check-expect (game-over? sigs-ex3) #t)

(define (game-over? s)
  (or (ufo-hit? (sigs-ufo s) (sigs-missile/#f s))
      (ufo-landed? (posn-y (sigs-ufo s)))))

;; inspired by yugoego's solution:
;; UFOCoordinate MissleOrNot -> Boolean
;; checks if UFO was hit by MISSILE.
(check-expect (ufo-hit? (make-posn 20 20) (make-posn 30 40)) #f)
(check-expect (ufo-hit? (make-posn 20 20) (make-posn 20 22)) #t)

(define (ufo-hit? su sm/#f)
  (if (boolean? sm/#f)
      #f
      (<= (distance-between su sm/#f) HIT-DISTANCE)))

;; inspired by yugoego's solution:
;; Posn Posn -> Number
;; determines the distance between two objects.
(check-expect (distance-between (make-posn 20 20) (make-posn 30 40)) 22)
(check-expect (distance-between (make-posn 20 20) (make-posn 20 22)) 2)

(define (distance-between p1 p2)
  (integer-sqrt
   (round (+ (expt (- (posn-x p1) (posn-x p2)) 2)
             (expt (- (posn-y p1) (posn-y p2)) 2)))))

;; MissileCoordinate -> Boolean
;; checks if UFO has reached EARTH.
(check-expect (ufo-landed? (posn-y (sigs-ufo sigs-ex3))) #t)
(check-expect (ufo-landed? (posn-y (sigs-ufo sigs-ex0))) #f)

(define (ufo-landed? su-y)
  (>= su-y EARTH))

;; SIGS -> Image
;; renders game-over screen when game stops. game-over screen
;; can be one of the following:
;; - "you lost." if UFO reaches EARTH without getting hit
;; - "you win." if UFO gets hit before reaching EARTH.
(check-expect (si-render-final sigs-ex3) (overlay USER-LOST (si-render sigs-ex3)))
(check-expect (si-render-final sigs-ex2) (overlay USER-WON (si-render sigs-ex2)))

(define (si-render-final s)
  (cond [(ufo-hit? (sigs-ufo s) (sigs-missile/#f s))
         (overlay USER-WON (si-render s))]
        [(ufo-landed? (posn-y (sigs-ufo s)))
         (overlay USER-LOST (si-render s))]))

;; Any -> SIGS
;; is s an example of the SIGS collection
(check-expect (sigs-given? sigs-ex3) #t)
(check-expect (sigs-given? (make-sigs (make-posn 1 2) (make-tank 1 2) #t)) #f)
(check-expect (sigs-given? #f) #f)

(define (sigs-given? s)
  (if (not
       (and (sigs? s)
            (posn? (sigs-ufo s))
            (tank? (sigs-tank s))
            (or (false? (sigs-missile/#f s)) (posn? (sigs-missile/#f s)))))
      #f
      #t))

;; SIGS -> SIGS
;; main function.
(define (si-main s)
  (big-bang s
            [to-draw si-render]
            [on-tick si-move]
            [on-key si-control]
            [stop-when game-over? si-render-final]
            [check-with sigs-given?]))

(test)
(si-main GAME-INIT-STATE)

;; original ufo-hit? function
;; UFOCoordinate MissileOrNot -> Boolean
;; checks if the tip of MISSILE has hit UFO. conditions
;; for hit:
;; - x-coordinate of MISSILE falls within width of UFO
;; - tip of MISSILE falls within height of UFO
;; (check-expect (ufo-hit? (fired-ufo-cor sigs-ex2) (fired-missile-cor sigs-ex2)) #t)
;; (check-expect (ufo-hit? (fired-ufo-cor (make-fired (make-posn 20 100)
;;                                                    (make-tank 100 TANK-VELOCITY-RIGHT)
;;                                                    (make-posn 40 143)))
;;                         (fired-missile-cor (make-fired (make-posn 20 100)
;;                                                        (make-tank 100 TANK-VELOCITY-RIGHT)
;;                                                        (make-posn 40 143)))) #f)

;; (define (ufo-hit? uc mc)
;;   (and (<= (- (posn-x uc) (/ (image-width UFO) 2))
;;            (posn-x mc)
;;            (+ (posn-x uc) (/ (image-width UFO) 2)))
;;        (<= (- (posn-y uc) (/ (image-height UFO) 2))
;;            (- (posn-y mc) (/ (image-height MISSILE) 2))
;;            (+ (posn-y uc) (/ (image-height UFO) 2)))))
