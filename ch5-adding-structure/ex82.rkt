#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;; a HangmanGuess is either a:
;; - 1String limited to letters from "a" through "z"
;; - #false
;; interpretation: represents an instance of user input in a
;; game of Hangman; #false means the input is wrong, else correct

(define-struct hangman-attempt [char-1 char-2 char-3])
;; a HangmanAttempt is a structure:
;;  (make-hangman-entry HangmanGuess HangmanGuess HangmanGuess)

(define hangman-attempt-example-1 (make-hangman-attempt "c" "a" "t"))
(define hangman-attempt-example-2 (make-hangman-attempt "#false" "a" "t"))
(define hangman-attempt-example-3 (make-hangman-attempt "k" "a" "t"))
(define hangman-attempt-example-4 (make-hangman-attempt "a" "c" "t"))

;; String String -> String
;; compares two words. if guesses match, the full word is returned;
;; else, the word is returned with the mismatched letters replaced with #f.
(check-expect (compare-words "cat" "cat") "cat")
(check-expect (compare-words "cat" "kat") "#false a t")
(check-expect (compare-words "cat" "act") "#false #false t")

(define (compare-words word-1 word-2)
  (cond [(string=? word-1 word-2) word-1]
        [else (compare-hangman-attempts (word->hangman-attempt word-1)
                                        (word->hangman-attempt word-2))]))

;; String -> HangmanAttempt
;; converts a string to a HangmanAttempt
(check-expect (word->hangman-attempt "cat") (make-hangman-attempt "c" "a" "t"))
(check-expect (word->hangman-attempt "kat") (make-hangman-attempt "k" "a" "t"))
(check-expect (word->hangman-attempt "act") (make-hangman-attempt "a" "c" "t"))

(define (word->hangman-attempt word)
  (make-hangman-attempt (substring word 0 1)
                        (substring word 1 2)
                        (substring word 2 3)))

;; HangmanAttempt -> String
;; compares two HangmanAttempts
(check-expect (compare-hangman-attempts hangman-attempt-example-1
                                        hangman-attempt-example-3)
              "#false a t")
(check-expect (compare-hangman-attempts hangman-attempt-example-1
                                        hangman-attempt-example-4)
              "#false #false t")

(define (compare-hangman-attempts hangman-attempt-1 hangman-attempt-2)
  (string-append (if (string=? (hangman-attempt-char-1 hangman-attempt-1)
                               (hangman-attempt-char-1 hangman-attempt-2))
                     (string-append (hangman-attempt-char-1 hangman-attempt-1) " ")
                     "#false ")
                 (if (string=? (hangman-attempt-char-2 hangman-attempt-1)
                               (hangman-attempt-char-2 hangman-attempt-2))
                     (string-append (hangman-attempt-char-2 hangman-attempt-1) " ")
                     "#false ")
                 (if (string=? (hangman-attempt-char-3 hangman-attempt-1)
                               (hangman-attempt-char-3 hangman-attempt-2))
                     (hangman-attempt-char-3 hangman-attempt-1)
                     "#false")))

(test)
