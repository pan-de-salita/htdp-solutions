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

;; SIGS -> SIGS
;; determines the position UFO, TANK, and MISSILE (if any)
;; per clock tick.
(check-random (si-move sigs-ex0) (si-move-proper sigs-ex0 (random (- UFO-WARP) UFO-WARP)))

(define (si-move s)
  (si-move-proper s (random (- UFO-WARP) UFO-WARP)))

;; SIGS Number -> SIGS
;; moves the space-invader objects predictably by delta.
(check-expect (si-move-proper sigs-ex0 4)
              (make-aim (make-posn (+ (posn-x (aim-ufo-cor sigs-ex0)) 4)
                                   (+ (posn-y (aim-ufo-cor sigs-ex0)) UFO-VELOCITY))
                        (make-tank (+ (tank-x (aim-tank-cor sigs-ex0)) (tank-dx (aim-tank-cor sigs-ex0)))
                                   (tank-dx (aim-tank-cor sigs-ex0)))))
(check-expect (si-move-proper sigs-ex1 2)
              (make-fired (make-posn (+ (posn-x (fired-ufo-cor sigs-ex1)) 2)
                                     (+ (posn-y (fired-ufo-cor sigs-ex1)) UFO-VELOCITY))
                          (make-tank (+ (tank-x (fired-tank-cor sigs-ex1)) (tank-dx (fired-tank-cor sigs-ex1)))
                                     (tank-dx (fired-tank-cor sigs-ex1)))
                          (make-posn (posn-x (fired-missile-cor sigs-ex1))
                                     (- (posn-y (fired-missile-cor sigs-ex1)) MISSILE-VELOCITY))))

(define (si-move-proper s delta)
  (cond [(aim? s)
         (make-aim (make-posn (if (within-frame? UFO-X-LIMIT-LEFT
                                                 (posn-x (aim-ufo-cor s))
                                                 UFO-X-LIMIT-RIGHT)
                                  (+ (posn-x (aim-ufo-cor s)) delta)
                                  (- (posn-x (aim-ufo-cor s)) delta UFO-WARP))
                              (+ (posn-y (aim-ufo-cor s)) UFO-VELOCITY))
                   (make-tank (+ (tank-x (aim-tank-cor s)) (tank-dx (aim-tank-cor s)))
                              (cond [(> (tank-x (aim-tank-cor s)) TANK-X-LIMIT-RIGHT) TANK-VELOCITY-LEFT]
                                    [(< (tank-x (aim-tank-cor s)) TANK-X-LIMIT-LEFT) TANK-VELOCITY-RIGHT]
                                    [else (tank-dx (aim-tank-cor s))])))]
        [(fired? s)
         (make-fired (make-posn (if (within-frame? UFO-X-LIMIT-LEFT
                                                   (posn-x (fired-ufo-cor s))
                                                   UFO-X-LIMIT-RIGHT)
                                    (+ (posn-x (fired-ufo-cor s)) delta)
                                    (- (posn-x (fired-ufo-cor s)) delta))
                                (+ (posn-y (fired-ufo-cor s)) UFO-VELOCITY))
                     (make-tank (+ (tank-x (fired-tank-cor s)) (tank-dx (fired-tank-cor s)))
                                (cond [(> (tank-x (fired-tank-cor s)) TANK-X-LIMIT-RIGHT) TANK-VELOCITY-LEFT]
                                      [(< (tank-x (fired-tank-cor s)) TANK-X-LIMIT-LEFT) TANK-VELOCITY-RIGHT]
                                      [else (tank-dx (fired-tank-cor s))]))
                     (make-posn (posn-x (fired-missile-cor s))
                                (- (posn-y (fired-missile-cor s)) MISSILE-VELOCITY)))]))

;; Number -> Boolean
;; checks if UFO is out of frame.
(check-expect (within-frame? UFO-X-LIMIT-LEFT 0 UFO-X-LIMIT-RIGHT) #f)
(check-expect (within-frame? UFO-X-LIMIT-LEFT CANVAS-WIDTH UFO-X-LIMIT-RIGHT) #f)
(check-expect (within-frame? UFO-X-LIMIT-LEFT 44 UFO-X-LIMIT-RIGHT) #t)

(define (within-frame? left-limit object right-limit)
  (< left-limit object right-limit))

;; SIGS KeyEvent -> SIGS
;; changes direction of TANK and launches MISSILE
;; depending on user's keyboard input:
;; - "left" | makes TANK go left
;; - "right" | makes TANK go right
;; - " " | fires MISSILE if not yet fired
(check-expect (si-control sigs-ex0 "left") (aim-control sigs-ex0 "left"))
(check-expect (si-control sigs-ex0 "right") (aim-control sigs-ex0 "right"))
(check-expect (si-control sigs-ex0 " ") (aim-control sigs-ex0 " "))
(check-expect (si-control sigs-ex0 "j") (aim-control sigs-ex0 "j"))
(check-expect (si-control sigs-ex1 "left") (fired-control sigs-ex1 "left"))
(check-expect (si-control sigs-ex1 "right") (fired-control sigs-ex1 "right"))
(check-expect (si-control sigs-ex1 " ") (fired-control sigs-ex1 " "))
(check-expect (si-control sigs-ex1 "k") (fired-control sigs-ex1 "k"))

(define (si-control s ke)
  (cond [(aim? s) (aim-control s ke)]
        [(fired? s) (fired-control s ke)]))

;; Aim -> SIGS
;; specific to Aims; changes direction of TANK and
;; launches MISSILE depending on user's keyboard input:
;; - "left" | makes TANK go left
;; - "right" | makes TANK go right
;; - " " | fires MISSILE if not yet fired; also converts
;;   given Aim into Fired
(check-expect (aim-control sigs-ex0 "left")
              (make-aim (make-posn (posn-x (aim-ufo-cor sigs-ex0))
                                   (posn-y (aim-ufo-cor sigs-ex0)))
                        (make-tank (tank-x (aim-tank-cor sigs-ex0))
                                   (if (= (tank-dx (aim-tank-cor sigs-ex0)) TANK-VELOCITY-RIGHT)
                                       TANK-VELOCITY-LEFT
                                       (tank-dx (aim-tank-cor sigs-ex0))))))
(check-expect (aim-control sigs-ex0 "right")
              (make-aim (make-posn (posn-x (aim-ufo-cor sigs-ex0))
                                   (posn-y (aim-ufo-cor sigs-ex0)))
                        (make-tank (tank-x (aim-tank-cor sigs-ex0))
                                   (if (= (tank-dx (aim-tank-cor sigs-ex0)) TANK-VELOCITY-LEFT)
                                       TANK-VELOCITY-RIGHT
                                       (tank-dx (aim-tank-cor sigs-ex0))))))
(check-expect (aim-control sigs-ex0 " ")
              (make-fired (make-posn (posn-x (aim-ufo-cor sigs-ex0))
                                     (posn-y (aim-ufo-cor sigs-ex0)))
                          (make-tank (tank-x (aim-tank-cor sigs-ex0))
                                     (tank-dx (aim-tank-cor sigs-ex0)))
                          (make-posn (tank-x (aim-tank-cor sigs-ex0))
                                     MISSILE-INIT-Y)))
(check-expect (aim-control sigs-ex0 "j") sigs-ex0)

(define (aim-control a ke)
  (cond [(key=? ke "left") (make-aim (make-posn (posn-x (aim-ufo-cor a))
                                                (posn-y (aim-ufo-cor a)))
                                     (make-tank (tank-x (aim-tank-cor a))
                                                (if (= (tank-dx (aim-tank-cor a)) TANK-VELOCITY-RIGHT)
                                                    TANK-VELOCITY-LEFT
                                                    (tank-dx (aim-tank-cor a)))))]
        [(key=? ke "right") (make-aim (make-posn (posn-x (aim-ufo-cor a))
                                                 (posn-y (aim-ufo-cor a)))
                                      (make-tank (tank-x (aim-tank-cor a))
                                                 (if (= (tank-dx (aim-tank-cor a)) TANK-VELOCITY-LEFT)
                                                     TANK-VELOCITY-RIGHT
                                                     (tank-dx (aim-tank-cor a)))))]
        [(key=? ke " ") (make-fired (make-posn (posn-x (aim-ufo-cor a))
                                               (posn-y (aim-ufo-cor a)))
                                    (make-tank (tank-x (aim-tank-cor a))
                                               (tank-dx (aim-tank-cor a)))
                                    (make-posn (tank-x (aim-tank-cor a))
                                               MISSILE-INIT-Y))]
        [else a]))

;; Fired -> Fired
;; specific to Fired; changes direction of TANK and
;; depending on user's keyboard input:
;; - "left" | makes TANK go left
;; - "right" | makes TANK go right
(check-expect (fired-control sigs-ex1 "left")
              (make-fired (make-posn (posn-x (fired-ufo-cor sigs-ex1))
                                     (posn-y (fired-ufo-cor sigs-ex1)))
                          (make-tank (tank-x (fired-tank-cor sigs-ex1))
                                     (if (= (tank-dx (fired-tank-cor sigs-ex1)) TANK-VELOCITY-RIGHT)
                                         TANK-VELOCITY-LEFT
                                         (tank-dx (fired-tank-cor sigs-ex1))))
                          (make-posn (posn-x (fired-missile-cor sigs-ex1))
                                     (posn-y (fired-missile-cor sigs-ex1)))))
(check-expect (fired-control sigs-ex1 "right")
              (make-fired (make-posn (posn-x (fired-ufo-cor sigs-ex1))
                                     (posn-y (fired-ufo-cor sigs-ex1)))
                          (make-tank (tank-x (fired-tank-cor sigs-ex1))
                                     (if (= (tank-dx (fired-tank-cor sigs-ex1)) TANK-VELOCITY-LEFT)
                                         TANK-VELOCITY-RIGHT
                                         (tank-dx (fired-tank-cor sigs-ex1))))
                          (make-posn (posn-x (fired-missile-cor sigs-ex1))
                                     (posn-y (fired-missile-cor sigs-ex1)))))
(check-expect (fired-control sigs-ex1 " ") sigs-ex1)
(check-expect (fired-control sigs-ex1 "k") sigs-ex1)

(define (fired-control f ke)
  (cond [(key=? ke "left") (make-fired (make-posn (posn-x (fired-ufo-cor f))
                                                  (posn-y (fired-ufo-cor f)))
                                       (make-tank (tank-x (fired-tank-cor f))
                                                  (if (= (tank-dx (fired-tank-cor f)) TANK-VELOCITY-RIGHT)
                                                      TANK-VELOCITY-LEFT
                                                      (tank-dx (fired-tank-cor f))))
                                       (make-posn (posn-x (fired-missile-cor f))
                                                  (posn-y (fired-missile-cor f))))]
        [(key=? ke "right") (make-fired (make-posn (posn-x (fired-ufo-cor f))
                                                  (posn-y (fired-ufo-cor f)))
                                       (make-tank (tank-x (fired-tank-cor f))
                                                  (if (= (tank-dx (fired-tank-cor f)) TANK-VELOCITY-LEFT)
                                                      TANK-VELOCITY-RIGHT
                                                      (tank-dx (fired-tank-cor f))))
                                       (make-posn (posn-x (fired-missile-cor f))
                                                  (posn-y (fired-missile-cor f))))]
        [else f]))

;; SIGS -> SIGS
;; main function.
(define (si-main s)
  (big-bang s
            [to-draw si-render]
            [on-tick si-move]
            [on-key si-control]
            [stop-when game-over? si-render-final]))

(si-main GAME-INIT-STATE)
(test)
