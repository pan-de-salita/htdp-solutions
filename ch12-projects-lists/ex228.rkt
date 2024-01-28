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
(define MESSAGE-3 "state-next-find: FSM-State not found: ")

(struct transition [current next] #:transparent)
;; a Transtition is a structure:
;;   (transition FSM-State FSM-State)

;; an FSM-State is a Color.

;; an FSM is one of:
;; - '()
;; - (cons Transition FSM)
;; an FSM represents the transitions that a finite
;; state machine can take from one state to another
;; in reaction to keystrokes.
;; examples:
(define fsm-traffic
  (list (transition "red" "green")
        (transition "green" "yellow")
        (transition "yellow" "red")))
(define fsm-black/white
  (list (transition "black" "white")
        (transition "white" "black")))

;; a Simulation-State.V1 is an FSM-State.

(struct finite-state [fsm current] #:transparent)
;; a Simulation-State.V2 is a structure:
;;   (finite-state FSM FSM-State)

;;;; functions

;; FSM -> Simulation-State.V2
;; match the keys pressed with the given FSM.

(define (simulate an-fsm)
  (big-bang (finite-state an-fsm (transition-current (car an-fsm)))
            [to-draw state-as-colored-square]
            [on-key state-next]))

;; Simulation-State.V2 -> Image
;; renders the current state as an image.

(check-expect
 (state-as-colored-square (finite-state fsm-traffic "red"))
 (empty-scene SCENE-WIDTH SCENE-HEIGHT "red"))
(check-expect
 (state-as-colored-square (finite-state fsm-traffic "green"))
 (empty-scene SCENE-WIDTH SCENE-HEIGHT "green"))
(check-expect
 (state-as-colored-square (finite-state fsm-traffic "yellow"))
 (empty-scene SCENE-WIDTH SCENE-HEIGHT "yellow"))
(check-expect
 (state-as-colored-square (finite-state fsm-black/white "black"))
 (empty-scene SCENE-WIDTH SCENE-HEIGHT "black"))
(check-expect
 (state-as-colored-square (finite-state fsm-black/white "white"))
 (empty-scene SCENE-WIDTH SCENE-HEIGHT "white"))

(define (state-as-colored-square an-fsm)
  (empty-scene SCENE-WIDTH SCENE-HEIGHT (finite-state-current an-fsm)))

;; Simulation-State.V2 Key-Event -> Simulation-State.V2
;; finds the next state from the current state and the Key-Event.

(check-expect (state-next (finite-state fsm-traffic "red") "a") (finite-state fsm-traffic "green"))
(check-expect (state-next (finite-state fsm-traffic "green") "e") (finite-state fsm-traffic "yellow"))
(check-expect (state-next (finite-state fsm-traffic "yellow") "i") (finite-state fsm-traffic "red"))
(check-expect (state-next (finite-state fsm-black/white "black") "o") (finite-state fsm-black/white "white"))
(check-expect (state-next (finite-state fsm-black/white "white") "u") (finite-state fsm-black/white "black"))

(define (state-next an-fsm key-event)
  (finite-state (finite-state-fsm an-fsm)
                (state-next-find (finite-state-fsm an-fsm) (finite-state-current an-fsm))))

;; FSM FSM-State -> FSM-State
;; finds the next state from the current FSM and FSM-State.

(check-expect (state-next-find fsm-black/white "black") "white")
(check-expect (state-next-find fsm-black/white "white") "black")
(check-expect (state-next-find fsm-traffic "red") "green")
(check-expect (state-next-find fsm-traffic "green") "yellow")
(check-expect (state-next-find fsm-traffic "yellow") "red")
(check-error (state-next-find fsm-traffic "black") (string-append MESSAGE-3 "black"))
(check-error (state-next-find fsm-black/white "red") (string-append MESSAGE-3 "red"))

(define (state-next-find fsm fsm-state)
  (cond [(empty? fsm) (error (string-append MESSAGE-3 fsm-state))]
        [else (cond [(state=? fsm-state (transition-current (car fsm))) (transition-next (car fsm))]
                    [(state-next-find (cdr fsm) fsm-state)])]))

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

;; (simulate fsm-traffic)
;; (simulate fsm-black/white)
