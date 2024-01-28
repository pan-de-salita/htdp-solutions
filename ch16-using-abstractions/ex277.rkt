#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;;;; constants and data definitions

(define UFO (bitmap "images/ollie.png")) ;; 50x50 px
(define UFO-WIDTH (image-width UFO))
(define UFO-HEIGHT (image-height UFO))

(define CANNON (bitmap "images/grumpy.png")) ;; same dimensions as UFO
(define CANNON-WIDTH (image-width CANNON))
(define CANNON-HEIGHT (image-height CANNON))

(define MISSILE (text/font "NO" 30 "red"
                           #f "default" "italic" "bold" #f)) ;; 25x25 px
(define MISSILE-WIDTH (image-width MISSILE))
(define MISSILE-HEIGHT (image-height MISSILE))

(define SCENE-WIDTH (* UFO-HEIGHT 5))
(define SCENE-HEIGHT (* UFO-WIDTH 8))
(define SCENE (empty-scene SCENE-WIDTH SCENE-HEIGHT "dimgrey"))

(define UFO-RETREATED (- 0 (/ UFO-HEIGHT 2)))
(define UFO-LANDED (- SCENE-HEIGHT (/ UFO-HEIGHT 2)))

(struct frame [ufo cannon missiles] #:transparent)
;; a Frame is a structure:
;;   (frame Posn Number List-of-Posns);
;; one instance of which describes the following:
;; - a UFO's Posn
;; - a cannone's Posn
;; - the Posns of every missile fired.
(define UFO-POSN-INIT (make-posn (/ SCENE-WIDTH 2) (/ SCENE-HEIGHT 4)))
(define CANNON-X-INIT (/ SCENE-WIDTH 2))
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
;; - MISSILEs

(define (game-render game-state)
  (local ((define CANNON-Y (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
          ;; Posn Number -> Image
          ;; renders assets of static quantity, i.e. UFO and CANNON
          ;; onto an-image
          (define (assets/static-quantity ufo-posn cannon-x an-image)
            (place-images (list UFO CANNON)
                          (list ufo-posn (make-posn cannon-x CANNON-Y))
                          an-image))
          ;; [List-of Posn] -> Image
          ;; renders assets of variable quantity, i.e. MISSILEs
          ;; onto SCENE
          (define (assets/variable-quantity missile-posns)
            (foldr
             (lambda (missile-posn an-image)
               (place-images (list MISSILE) (list missile-posn) an-image))
             SCENE
             missile-posns)))
    (assets/static-quantity (frame-ufo game-state)
                            (frame-cannon game-state)
                            (assets/variable-quantity (frame-missiles game-state)))))

;; Game-State -> Game-State
;; move the following per clock tick:
;; - UFO
;; - missiles

(define (game-move game-state)
  (local ((define UFO-HIT-RANGE (+ (/ UFO-WIDTH 2) (/ MISSILE-WIDTH 2)))
          ;; checks if any MISSILE is close enough to UFO
          ;; using the Pythagorean theorem
          (define (within-range-of-ufo? missile-posn)
            (<= (sqrt (+ (sqr (- (posn-x (frame-ufo game-state)) (posn-x missile-posn)))
                         (sqr (- (posn-y (frame-ufo game-state)) (posn-y missile-posn)))))
                UFO-HIT-RANGE))
          ;; Posn [List-of Posn] -> Posn
          ;; determines the new Posn of UFO
          (define (ufo-move ufo-posn/old missiles-posn/old)
            (local (;; Number -> Number
                    ;; determines the new x-coordinate of UFO
                    (define (ufo-move/x ufo-x/old)
                      (local ((define UFO-LIMIT-LEFT (/ UFO-WIDTH 2))
                              (define UFO-LIMIT-RIGHT (- SCENE-WIDTH (/ UFO-WIDTH 2)))
                              (define UFO-WARP-RANGE-MIN 1)
                              (define UFO-WARP-RANGE-MAX 30)
                              (define UFO-ADJUSTMENT-X 30)
                              (define ufo-x/warped
                                ((if (= (random 2) 1) + -)
                                 (posn-x ufo-posn/old) (random UFO-WARP-RANGE-MIN UFO-WARP-RANGE-MAX))))
                        (cond [(<= ufo-x/warped UFO-LIMIT-LEFT) (+ ufo-x/warped UFO-ADJUSTMENT-X)]
                              [(>= ufo-x/warped UFO-LIMIT-RIGHT) (- ufo-x/warped UFO-ADJUSTMENT-X)]
                              [else ufo-x/warped])))
                    ;; Posn [List-of Posn] -> Number
                    ;; determines the new y-coordinate of UFO:
                    ;; - if a MISSILE hits UFO, moves UFO upward by UFO-VELOCITY-RETREATING
                    ;; - else, moves UFO by UFO-VELOCITY-LANDING from the top of SCENE
                    (define (ufo-move/y missiles-posn)
                      (local ((define UFO-VELOCITY-LANDING 2)
                              (define UFO-VELOCITY-RETREATING 40)
                              ;; Posn [List-of Posn] -> Boolean
                              ;; checks if any MISSILE has hit UFO
                              (define (ufo-hit? l-missiles)
                                (ormap within-range-of-ufo? l-missiles)))
                        (cond [(ufo-hit? missiles-posn) (- (posn-y ufo-posn/old) UFO-VELOCITY-RETREATING)]
                              [else (+ (posn-y ufo-posn/old) UFO-VELOCITY-LANDING)]))))
              (make-posn (ufo-move/x (posn-x ufo-posn/old))
                         (ufo-move/y missiles-posn/old))))
          (define MISSILES-Y-INIT (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
          (define MISSILES-Y-HIT (- 0 MISSILE-HEIGHT))
          (define MISSILE-VELOCITY 50)
          ;; [List-of Posn] -> [List-of Posn]
          ;; determines the new Posns of MISSILES
          (define (missiles-move missiles-posn/old)
            (map
             (lambda (missile-posn)
               (cond [(within-range-of-ufo? missile-posn) (make-posn (posn-x missile-posn) MISSILES-Y-HIT)]
                     [else (make-posn (posn-x missile-posn) (- (posn-y missile-posn) MISSILE-VELOCITY))]))
             missiles-posn/old)))
    (frame (ufo-move (frame-ufo game-state) (frame-missiles game-state))
           (frame-cannon game-state)
           (missiles-move (frame-missiles game-state)))))

;; Game-State Key-Event -> Game-State
;; does the following according to Key-Event:
;; - "left" or "right": moves CANNON left or right on SCENE
;; - "space": launches a missile

(define (game-control game-state key-event)
  (local ((define CANNON-LIMIT-LEFT (/ CANNON-WIDTH 2))
          (define CANNON-LIMIT-RIGHT (- SCENE-WIDTH (/ CANNON-WIDTH 2)))
          (define CANNON-VELOCITY 10)
          ;; Number -> Number
          ;; moves CANNON left or right according to Key-Event
          (define (cannon-move cannon-x-cor)
            (cond [(and (key=? key-event "left") (>= cannon-x-cor CANNON-LIMIT-LEFT))
                   (- cannon-x-cor CANNON-VELOCITY)]
                  [(and (key=? key-event "right") (<= cannon-x-cor CANNON-LIMIT-RIGHT))
                   (+ cannon-x-cor CANNON-VELOCITY)]
                  [else cannon-x-cor]))
          (define MISSILES-Y-INIT (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
          ;; Number [List-of Posn] -> [List-of Posn
          ;; launches a missile at CANNON'S Posn when Key-Event is " "
          (define (missiles-launch cannon-posn missiles)
            (cond [(not (key=? key-event " ")) missiles]
                  [else (cons (make-posn cannon-posn MISSILES-Y-INIT) missiles)])))
    (frame (frame-ufo game-state)
           (cannon-move (frame-cannon game-state))
           (missiles-launch (frame-cannon game-state) (frame-missiles game-state)))))

;; Game-State -> Boolean
;; checks either of the following:
;; - has UFO retreated?
;; - has UFO landed?

(define (game-over? game-state)
  (or (<= (posn-y (frame-ufo game-state)) UFO-RETREATED)
      (>= (posn-y (frame-ufo game-state)) UFO-LANDED)))

;; Game-State -> Image
;; renders the game-over screen

(define (game-over-render game-state)
  (local ((define USER-WINS "m e a n i e.")
          (define USER-LOSES "h e ' s   h e r e.")
          (define game-over-text
            (cond [(<= (posn-y (frame-ufo game-state)) UFO-RETREATED) USER-WINS]
                  [else USER-LOSES]))
          (define game-over-text-window (rectangle 200 56 "solid" "black")))
    (overlay (text game-over-text 25 "white")
             game-over-text-window
             (game-render game-state))))

;;;; tests

;; game-render
;; -------------------------------
(check-expect
 (game-render FRAME-INIT)
 (place-images
  (list UFO CANNON)
  (list (frame-ufo FRAME-INIT)
        (make-posn (frame-cannon FRAME-INIT) (- SCENE-HEIGHT (/ CANNON-HEIGHT 2))))
  (foldr (lambda (missile-posn receiving-image)
           (place-images (list MISSILE) (list missile-posn) receiving-image))
         SCENE (frame-missiles FRAME-INIT))))
(check-expect
 (game-render
  (frame (make-posn 148 89)
         23
         (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
               (make-posn 100 158)
               (make-posn 122 95))))
 (place-images
  (list UFO CANNON)
  (list (make-posn 148 89)
        (make-posn 23 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2))))
  (foldr (lambda (missile-posn receiving-image)
           (place-images (list MISSILE) (list missile-posn) receiving-image))
         SCENE (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
                     (make-posn 100 158)
                     (make-posn 122 95)))))

;; game-move
;; -------------------------------
;; Game-State -> Game-State
;; functions just as game-move does
;; NOTE: for testing game-move only

(check-random (game-move/test FRAME-INIT) (game-move FRAME-INIT))
(check-random
 (game-move/test
  (frame (make-posn 148 89)
         23
         (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
               (make-posn 100 158)
               (make-posn 122 95))))
 (game-move
  (frame (make-posn 148 89)
         23
         (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
               (make-posn 100 158)
               (make-posn 122 95)))))

(define (game-move/test game-state)
  (game-move game-state))

;; game-control
;; -------------------------------
(check-expect (game-control FRAME-INIT "left") (frame (make-posn 200 160) 190 '()))
(check-expect (game-control FRAME-INIT "right") (frame (make-posn 200 160) 210 '()))
(check-expect (game-control FRAME-INIT " ") (frame (make-posn 200 160) 200 (list (make-posn 200 600))))
(check-expect (game-control FRAME-INIT "p") FRAME-INIT)

;; game-over?
;; -------------------------------
(check-expect (game-over? FRAME-INIT) #f)
(check-expect
 (game-over?
  (frame (make-posn 89 -50)
         56
         (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
               (make-posn 100 158)
               (make-posn 122 95))))
 #t)
(check-expect
 (game-over?
  (frame (make-posn 89 UFO-LANDED)
         56
         (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
               (make-posn 100 158)
               (make-posn 122 95))))
 #t)

;; game-render
;; -------------------------------
(check-expect
 (game-over-render
  (frame (make-posn 89 -50)
         56
         (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
               (make-posn 100 158)
               (make-posn 122 95))))
 (overlay (text "m e a n i e." 25 "white")
          (rectangle 200 56 "solid" "black")
          (game-render
           (frame (make-posn 89 -50)
                  56
                  (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
                        (make-posn 100 158)
                        (make-posn 122 95))))))
(check-expect
 (game-over-render
  (frame (make-posn 89 UFO-LANDED)
         56
         (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
               (make-posn 100 158)
               (make-posn 122 95))))
 (overlay (text "h e ' s   h e r e." 25 "white")
          (rectangle 200 56 "solid" "black")
          (game-render
           (frame (make-posn 89 UFO-LANDED)
                  56
                  (list (make-posn 29 (- SCENE-HEIGHT (/ CANNON-HEIGHT 2)))
                        (make-posn 100 158)
                        (make-posn 122 95))))))

;;;; application

(test)
(ufo-main FRAME-INIT)
