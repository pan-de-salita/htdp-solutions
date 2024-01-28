#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)

;;;; constants and data definitions

(define MESSAGE-0 "state=? requires two colors. neither argument is a color.")
(define MESSAGE-1 "state=? requires two colors. the first argument is not a color.")
(define MESSAGE-2 "state=? requires two colors. the second argument is not a color.")

(struct transition [current next] #:transparent)
;; a Transtition is a structure:
;;   (transition FSM-State FSM-State)

;; an FSM-State is a color.

;; an FSM is one of:
;; - '()
;; - (cons Transition FSM)
;; examples:
(define fsm-traffic
  (list (transition "red" "green")
        (transition "green" "yellow")
        (transition "yellow" "red")))
(define fsm-black/white
  (list (transition "black" "white")
        (transition "white" "black")))

;; NOTE: an FSM represents the transitions that a
;; finite state machine can take from one state to
;; another in reaction to keystrokes.

;;;; functions

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
