#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;; a Correct-Letter? is either a:
;; - 1String (letters "a" through "z")
;; - #f
;; interpretation: represents an instance of user input
;; in a game of Hangman; #f means the input is wrong, else
;; correct

(define-struct hangman-entry [entry-1 entry-2 entry-3])
;; a Hangman-Entry is a structure:
;;  (make-hangman-entry Correct-Letter? Correct-Letter? Correct-Letter?)
