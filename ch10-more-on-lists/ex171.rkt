#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;; CONSTANTS ----------------------------------------------

;;; DATA DEFINITIONS ---------------------------------------

;; a List-of-strings is one of:
;; - '()
;; - (cons String List-of-strings)

;; a List-of-list-of-strings is one of:
;; - '()
;; - (cons List-of-strings List-of-list-of-strings)

;;; FUNCTIONS ----------------------------------------------

;; Piet Hein's "TTT" where each line represented as a string
(define TTT-LINES
  (cons "TTT"
        (cons ""
              (cons "Put up in a place"
                    (cons "where it's easy to see"
                          (cons "the cryptic admonishment"
                                (cons "T.T.T."
                                      (cons ""
                                            (cons "When you feel how depressingly"
                                                  (cons "slowly you climb,"
                                                        (cons "it's well to remember that"
                                                              (cons "Things Take Time."
                                                                    (cons ""
                                                                          (cons "Piet Hein" '()))))))))))))))

;; Piet Hein's "TTT" where each word is a string
(define TTT-WORDS
  (cons "TTT"
        (cons "Put"
              (cons "up"
                    (cons "in"
                          (cons "a"
                                (cons "place"
                                      (cons "where"
                                            (cons "it's"
                                                  (cons "easy"
                                                        (cons "to"
                                                              (cons "see"
                                                                    (cons "the"
                                                                          (cons "cryptic"
                                                                                (cons "admonishment"
                                                                                      (cons "T.T.T."
                                                                                            (cons "When"
                                                                                                  (cons "you"
                                                                                                        (cons "feel"
                                                                                                              (cons "how"
                                                                                                                    (cons "depressingly"
                                                                                                                          (cons "slowly"
                                                                                                                                (cons "you"
                                                                                                                                      (cons "climb,"
                                                                                                                                            (cons "it's"
                                                                                                                                                  (cons "well"
                                                                                                                                                        (cons "to"
                                                                                                                                                              (cons "remember"
                                                                                                                                                                    (cons "that"
                                                                                                                                                                          (cons "Things"
                                                                                                                                                                                (cons "Take"
                                                                                                                                                                                      (cons "Time."
                                                                                                                                                                                            (cons "Piet"
                                                                                                                                                                                                  (cons "Hein" '()))))))))))))))))))))))))))))))))))

;; Piet Hein's "TTT" where each line is a list of words
(define TTT-WORDS/LINE
  (cons (cons "TTT" '())
        (cons '()
              (cons (cons "Put"
                          (cons "up"
                                (cons "in"
                                      (cons "a"
                                            (cons "place" '())))))
                    (cons (cons "where"
                                (cons "it's"
                                      (cons "easy"
                                            (cons "to"
                                                  (cons "see" '())))))
                          (cons (cons "the"
                                      (cons "cryptic"
                                            (cons "admonishment" '())))
                                (cons (cons "T.T.T." '())
                                      (cons '()
                                            (cons (cons "When"
                                                        (cons "you"
                                                              (cons "feel"
                                                                    (cons "how"
                                                                          (cons "depressingly" '())))))
                                                  (cons (cons "slowly"
                                                              (cons "you"
                                                                    (cons "climb," '())))
                                                        (cons (cons "it's"
                                                                    (cons "well"
                                                                          (cons "to"
                                                                                (cons "remember"
                                                                                      (cons "that" '())))))
                                                              (cons (cons "Things"
                                                                          (cons "Take"
                                                                                (cons "Time." '())))
                                                                    (cons '()
                                                                          (cons (cons "Piet"
                                                                                      (cons "Hein" '())) '()))))))))))))))

(check-expect (read-lines "ttt.txt") TTT-LINES)
(check-expect (read-words "ttt.txt") TTT-WORDS)
(check-expect (read-words/line "ttt.txt") TTT-WORDS/LINE)

;;; APPLICATION --------------------------------------------

(test)
