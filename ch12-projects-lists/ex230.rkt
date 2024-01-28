#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;;;; constants and data definitions

(define SCENE-WIDTH 100)
(define SCENE-HEIGHT 100)

(define MESSAGE-0 "state=?: the function requires two colors. neither argument is a color.")
(define MESSAGE-1 "state=?: the function requires two colors. the first argument is not a color.")
(define MESSAGE-2 "state=?: the function requires two colors. the second argument is not a color.")

;; an FSM-State is a Color.

(struct transition [current key next] #:transparent)
;; a Transition is a structure:
;;   (make-transition FSM-State Key-Event FSM-State)
(define TRANSITION-INITIAL (transition "white" "a" "yellow"))
(define TRANSITION-B (transition "yellow" "b" "yellow"))
(define TRANSITION-C (transition "yellow" "c" "yellow"))
(define TRANSITION-FINAL (transition "yellow" "d" "green"))

;; a List-of-Transitions is on of:
;; - '()
;; - (cons Transition List-of-Transitions)
(define LIST-OF-TRANSITIONS-EX109
  (list TRANSITION-INITIAL
        TRANSITION-B
        TRANSITION-C
        TRANSITION-FINAL))

(struct fsm [initial transitions final] #:transparent)
;; an FSM is a structure:
;;   (fsm FSM-State LOT FSM-State)
(define FSM-EX109-INITIAL ;; initial state; no key-event given
  (fsm (transition-current TRANSITION-INITIAL)
       LIST-OF-TRANSITIONS-EX109
       (transition-next TRANSITION-FINAL)))
(define FSM-EX109-A ;; key-event: "a"
  (fsm (transition-current TRANSITION-B)
       LIST-OF-TRANSITIONS-EX109
       (transition-next TRANSITION-FINAL)))
(define FSM-EX109-B ;; key-event: "b"
  (fsm (transition-current TRANSITION-C)
       LIST-OF-TRANSITIONS-EX109
       (transition-next TRANSITION-FINAL)))
(define FSM-EX109-C ;; key-event: "c"
  (fsm (transition-current TRANSITION-FINAL)
       LIST-OF-TRANSITIONS-EX109
       (transition-next TRANSITION-FINAL)))
(define FSM-EX109-FINAL ;; key-event: "d"
  (fsm (transition-next TRANSITION-FINAL)
       LIST-OF-TRANSITIONS-EX109
       (transition-next TRANSITION-FINAL)))

;;;; functions

;; FSM -> FSM
;; main program.

(define (fsm-simulate an-fsm)
  (big-bang an-fsm
            [to-draw fsm/state->square]
            [on-key fsm/initial->next]
            [stop-when fsm/initial=final? fsm/state->square]))

;; FSM -> Image
;; renders the current state of an FSM as a colored square.

(check-expect
 (fsm/state->square FSM-EX109-INITIAL)
 (empty-scene SCENE-WIDTH SCENE-HEIGHT (fsm-initial FSM-EX109-INITIAL)))
(check-expect
 (fsm/state->square FSM-EX109-A)
 (empty-scene SCENE-WIDTH SCENE-HEIGHT (fsm-initial FSM-EX109-A)))
(check-expect
 (fsm/state->square FSM-EX109-B)
 (empty-scene SCENE-WIDTH SCENE-HEIGHT (fsm-initial FSM-EX109-B)))
(check-expect
 (fsm/state->square FSM-EX109-C)
 (empty-scene SCENE-WIDTH SCENE-HEIGHT (fsm-initial FSM-EX109-C)))
(check-expect
 (fsm/state->square FSM-EX109-FINAL)
 (empty-scene SCENE-WIDTH SCENE-HEIGHT (fsm-initial FSM-EX109-FINAL)))

(define (fsm/state->square an-fsm)
  (empty-scene SCENE-WIDTH SCENE-HEIGHT (fsm-initial an-fsm)))

;; FSM Key-Event -> FSM
;; determines the next state of an FSM according to its current
;; state and the user's keyboard input.

(check-expect (fsm/initial->next FSM-EX109-INITIAL "a") FSM-EX109-A)
(check-expect (fsm/initial->next FSM-EX109-A "b") FSM-EX109-B)
(check-expect (fsm/initial->next FSM-EX109-B "c") FSM-EX109-C)
(check-expect (fsm/initial->next FSM-EX109-C "d") FSM-EX109-FINAL)

(define (fsm/initial->next an-fsm a-key-event)
  (fsm (fsm/next (fsm-initial an-fsm) (fsm-transitions an-fsm) a-key-event)
       (fsm-transitions an-fsm)
       (fsm-final an-fsm)))

;; FSM-State List-of-Transitions Key-Event -> FSM-State
;; determines the next FSM-State according to the current state,
;; List-of-Transitions, and Key-Event given.

(check-expect (fsm/next "white" LIST-OF-TRANSITIONS-EX109 "a") "yellow")
(check-expect (fsm/next "yellow" LIST-OF-TRANSITIONS-EX109 "b") "yellow")
(check-expect (fsm/next "yellow" LIST-OF-TRANSITIONS-EX109 "c") "yellow")
(check-expect (fsm/next "yellow" LIST-OF-TRANSITIONS-EX109 "d") "green")
(check-error
 (fsm/next "white" LIST-OF-TRANSITIONS-EX109 "b")
 (message/fsm-state-not-found "white" "b"))
(check-error
 (fsm/next "white" LIST-OF-TRANSITIONS-EX109 "c")
 (message/fsm-state-not-found "white" "c"))
(check-error
 (fsm/next "white" LIST-OF-TRANSITIONS-EX109 "d")
 (message/fsm-state-not-found "white" "d"))
(check-error
 (fsm/next "yellow" LIST-OF-TRANSITIONS-EX109 "a")
 (message/fsm-state-not-found "yellow" "a"))

(define (fsm/next initial-fsm-state list-of-transitions a-key-event)
  (cond [(empty? list-of-transitions) (error (message/fsm-state-not-found initial-fsm-state a-key-event))]
        [else (cond [(and (string=? a-key-event (transition-key (car list-of-transitions)))
                          (state=? initial-fsm-state (transition-current (car list-of-transitions))))
                     (transition-next (car list-of-transitions))]
                    [else (fsm/next initial-fsm-state (cdr list-of-transitions) a-key-event)])]))

;; FSM-State Key-Event -> String
;; returns an error message for when a key-event does not point
;; to a corresponding FSM-State.

(check-expect
 (message/fsm-state-not-found "white" "z")
 (string-append "fsm/next: no next state for [" "white" "] with key-event [" "z" "]"))

(define (message/fsm-state-not-found an-fsm-state a-key-event)
  (string-append "fsm/next: no next state for [" an-fsm-state "] with key-event [" a-key-event "]"))

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

;; FSM -> Boolean
;; checks if the FSM has entered its final state.

(check-expect (fsm/initial=final? FSM-EX109-INITIAL) #f)
(check-expect (fsm/initial=final? FSM-EX109-A) #f)
(check-expect (fsm/initial=final? FSM-EX109-B) #f)
(check-expect (fsm/initial=final? FSM-EX109-C) #f)
(check-expect (fsm/initial=final? FSM-EX109-FINAL) #t)

(define (fsm/initial=final? an-fsm)
  (state=? (fsm-initial an-fsm) (fsm-final an-fsm)))

;;;; application

(test)
(fsm-simulate FSM-EX109-INITIAL)
