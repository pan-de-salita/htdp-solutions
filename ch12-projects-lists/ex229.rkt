#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;;;; constants and data definitions

(define SCENE-WIDTH 100)
(define SCENE-HEIGHT 100)

(define MESSAGE-0 "state=? requires two colors. neither argument is a color.")
(define MESSAGE-1 "state=? requires two colors. the first argument is not a color.")
(define MESSAGE-2 "state=? requires two colors. the second argument is not a color.")
(define MESSAGE-3 "state-next-find: corresponding FSM-State not found for keystroke: ")

;; an FSM-State is a Color.

(struct key-transition [current key next] #:transparent)
;; a Key-Transtition is a structure:
;;   (key-transition FSM-State Key-Event FSM-State)
(define KEY-TRANSITION-EXPECT-A (key-transition "white" "a" "yellow"))
(define KEY-TRANSITION-EXPECT-B (key-transition "yellow" "b" "yellow"))
(define KEY-TRANSITION-EXPECT-C (key-transition "yellow" "c" "yellow"))
(define KEY-TRANSITION-EXPECT-D (key-transition "yellow" "d" "white"))

;; an Key-FSM is one of:
;; - '()
;; - (cons Key-Transition FSM)
;; an FSM represents the transitions that a finite
;; state machine can take from one state to another
;; in reaction to keystrokes.
(define KEY-FSM-EX109
  (list KEY-TRANSITION-EXPECT-A
        KEY-TRANSITION-EXPECT-B
        KEY-TRANSITION-EXPECT-C
        KEY-TRANSITION-EXPECT-D))

(struct simulation-state [key-fsm current] #:transparent)
;; a Simulation-State is a structure:
;;   (finite-state Key-FSM FSM-State)

;;;; functions

;; Key-FSM -> Simulation-State
;; match the keys pressed with the given FSM.

(define (simulate a-key-fsm)
  (big-bang (simulation-state a-key-fsm (key-transition-current (car a-key-fsm)))
            [to-draw state-as-colored-square]
            [on-key state-next]))

;; Simulation-State -> Image
;; renders the current state as an image.

(check-expect
 (state-as-colored-square (simulation-state KEY-FSM-EX109 "white"))
 (empty-scene SCENE-WIDTH SCENE-HEIGHT "white"))
(check-expect
 (state-as-colored-square (simulation-state KEY-FSM-EX109 "orange"))
 (empty-scene SCENE-WIDTH SCENE-HEIGHT "orange"))
(check-expect
 (state-as-colored-square (simulation-state KEY-FSM-EX109 "lightyellow"))
 (empty-scene SCENE-WIDTH SCENE-HEIGHT "lightyellow"))
(check-expect
 (state-as-colored-square (simulation-state KEY-FSM-EX109 "yellow"))
 (empty-scene SCENE-WIDTH SCENE-HEIGHT "yellow"))

(define (state-as-colored-square a-simulation-state)
  (empty-scene SCENE-WIDTH SCENE-HEIGHT (simulation-state-current a-simulation-state)))

;; Simulation-State Key-Event -> Simulation-State.V2
;; finds the next state from the current state and the Key-Event.

(check-expect
 (state-next (simulation-state KEY-FSM-EX109 "white") "a")
 (simulation-state KEY-FSM-EX109 "yellow"))
(check-expect
 (state-next (simulation-state KEY-FSM-EX109 "yellow") "b")
 (simulation-state KEY-FSM-EX109 "yellow"))
(check-expect
 (state-next (simulation-state KEY-FSM-EX109 "yellow") "c")
 (simulation-state KEY-FSM-EX109 "yellow"))
(check-expect
 (state-next (simulation-state KEY-FSM-EX109 "yellow") "d")
 (simulation-state KEY-FSM-EX109 "white"))

(define (state-next a-simulation-state key-event)
  (simulation-state (simulation-state-key-fsm a-simulation-state)
                    (state-next-find (simulation-state-key-fsm a-simulation-state)
                                     key-event
                                     (simulation-state-current a-simulation-state))))

;; FSM FSM-State -> FSM-State
;; finds the next state from the current FSM and FSM-State.

(check-expect (state-next-find KEY-FSM-EX109 "a" "white") "yellow")
(check-expect (state-next-find KEY-FSM-EX109 "b" "yellow") "yellow")
(check-expect (state-next-find KEY-FSM-EX109 "c" "yellow") "yellow")
(check-expect (state-next-find KEY-FSM-EX109 "d" "yellow") "white")
(check-error (state-next-find KEY-FSM-EX109 "a" "yellow") (string-append MESSAGE-3 "a"))
(check-error (state-next-find KEY-FSM-EX109 "b" "white") (string-append MESSAGE-3 "b"))

(define (state-next-find a-key-fsm key-event current-state)
  (cond [(empty? a-key-fsm) (error (string-append MESSAGE-3 key-event))]
        [else (cond [(and (state=? current-state (key-transition-current (car a-key-fsm)))
                          (key=? key-event (key-transition-key (car a-key-fsm))))
                     (key-transition-next (car a-key-fsm))]
                    [else (state-next-find (cdr a-key-fsm) key-event current-state)])]))

;; Any Any -> Boolean
;; checks whether the two inputs are FSM-States, and
;; if so, whether the two FSM-States are equal.

(check-error (state=? 0 1) MESSAGE-0)
(check-error (state=? 0 "red") MESSAGE-1)
(check-error (state=? "red" 1) MESSAGE-2)
(check-expect (state=? "red" "red") #t)
(check-expect (state=? "red" "black") #f)

(define (state=? state-a state-b)
  (cond [(and (not (image-color? state-a)) (not (image-color? state-b))) (error MESSAGE-0)]
        [(not (image-color? state-a)) (error MESSAGE-1)]
        [(not (image-color? state-b)) (error MESSAGE-2)]
        [else (string=? state-a state-b)]))

;;;; application

(test)
(simulate KEY-FSM-EX109)
