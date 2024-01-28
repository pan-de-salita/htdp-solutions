#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;; data definitions

;; a Word is one of:
;; - '()
;; - (cons 1String Word)
;; i.e. a Word is an English word whose letters
;; are arranged within a list, one letter at a time.
;; examples:
(define WORD-EXAMPLE-0 '())
(define WORD-EXAMPLE-1 (list "c" "a" "t"))
(define WORD-EXAMPLE-2 (list "a" "c" "t"))
(define WORD-EXAMPLE-3 (list "t" "a" "c"))
(define WORD-EXAMPLE-4 (list "r" "a" "t"))
(define WORD-EXAMPLE-5 (list "t" "a" "r"))
(define WORD-EXAMPLE-6 (list "a" "r" "t"))
(define WORD-EXAMPLE-7 (list "t" "r" "a"))

;; a List-of-Words is one of:
;; - '()
;; - (cons Word List-of-Words)
;; i.e. a list of Words.
;; examples:
(define LIST-OF-WORDS-EXAMPLE-0 '())
(define LIST-OF-WORDS-EXAMPLE-1
  (list WORD-EXAMPLE-1
        WORD-EXAMPLE-2))
(define LIST-OF-WORDS-EXAMPLE-2
  (list WORD-EXAMPLE-1
        WORD-EXAMPLE-2
        WORD-EXAMPLE-3))
(define LIST-OF-WORDS-EXAMPLE-3
  (list WORD-EXAMPLE-4
        WORD-EXAMPLE-5
        WORD-EXAMPLE-6))
(define LIST-OF-WORDS-EXAMPLE-4
  (list WORD-EXAMPLE-4
        WORD-EXAMPLE-5
        WORD-EXAMPLE-6
        WORD-EXAMPLE-7))

;;; constants



;;; functions

;; String -> List-of-Strings
;; finds all words that use the same letters as s.

(check-satisfied (alternative-words "cat") all-words-from-cat?)
(check-satisfied (alternative-words "rat") all-words-from-rat?)

(define (alternative-words a-string)
  (in-dictionary (words->strings (arrangements (string->word a-string)))))

;; Word -> List-of-Words
;; finds all rearrangements of given Word.

;; (define (arrangements a-word)
;;   (list a-word))

;; String -> Word
;; converts a String to Word.

(check-expect (string->word "") '())
(check-expect (string->word "cat") (list "c" "a" "t"))

(define (string->word a-string)
  (if (= (string-length a-string) 0)
      '()
      (cons (substring a-string 0 1) (string->word (substring a-string 1)))))

;; Word -> String
;; converts a Word to a String.

(check-expect (word->string '()) "")
(check-expect (word->string (list "c" "a" "t")) "cat")

(define (word->string a-word)
  (if (empty? a-word)
      ""
      (string-append (first a-word) (word->string (rest a-word)))))

;; List-of-Strings -> Boolean
;; checks if all Strings in a List-of-Strings are
;; valid arrangements of "cat".

(check-expect (all-words-from-cat? (list "cat" "act")) #t)
(check-expect (all-words-from-cat? (list "cat" "tac")) #f)
(check-expect (all-words-from-cat? (list "dog" "rat")) #f)
(check-expect (all-words-from-cat? '()) #f)

(define (all-words-from-cat? a-list-of-strings)
  (and (not (boolean? (member "cat" a-list-of-strings)))
       (not (boolean? (member "act" a-list-of-strings)))))

;; List-of-Strings -> Boolean
;; checks if all Strings in a List-of-Strings are
;; valid arrangements of "rat".

(check-expect (all-words-from-rat? (list "rat" "tar" "art")) #t)
(check-expect (all-words-from-rat? (list "rat" "cat")) #f)
(check-expect (all-words-from-rat? (list "cat" "tac")) #f)
(check-expect (all-words-from-rat? '()) #f)

(define (all-words-from-rat? a-list-of-strings)
  (and (not (boolean? (member "rat" a-list-of-strings)))
       (not (boolean? (member "tar" a-list-of-strings)))
       (not (boolean? (member "art" a-list-of-strings)))))

;;; application

(test)
