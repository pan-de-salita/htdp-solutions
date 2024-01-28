#lang racket
(require test-engine/racket-tests
         2htdp/image
         2htdp/universe)

;;;; constants and data definitions

;; an FSM-State is a Color.

(define-struct transition [current next] #:transparent)
;; a Transition is a structure:
;;   (transition FSM-State FSM-State)

;; an FSM is one of:
;; - '()
;; - (cons Transition FSM)
;; an FSM represents the transitions that a finite
;; state machine can take from one state to another
;; in reaction to keystrokes.
(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

(define-struct sim-state [fsm current] #:transparent)
;; a Simulation-State is a structure:
;;   (sim-state Key-FSM FSM-State)

;;;; functions

;; FSM FSM-State -> FSM-State
;; matches the keys pressed by a user with the given FSM

(define (simulate fsm s0)
  (local (;; State of the world: FSM-State
          ;; FSM-State KeyEvent -> FSM-State
          (define (find-next-state s key-event)
            (find fsm s)))
    (big-bang s0
              [to-draw state-as-colored-square]
              [on-key find-next-state])))

;; FSM-State -> Image
;; renders current state as colored square

(define (state-as-colored-square s)
  (square 100 "solid" s))

;; FSM FMS-State -> FSM-State
;; finds the current state's successor in fsm

(define (find transitions current)
  (cond [(empty? transitions) (error "not found")]
        [else (local ((define s (first transitions)))
                (if (state=? (transition-current s) current)
                    (transition-next s)
                    (find (rest transitions) current)))]))

;; Any Any -> Boolean
;; checks whether the two inputs are FSM-States

(define (state=? state-a state-b)
  (string=? state-a state-b))

;;;; application

(simulate fsm-traffic (transition-current (car fsm-traffic)))
