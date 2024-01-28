#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;;; data definitions

;; a Letter is one of the following alphabetical 1Strings:
;; - "a"
;; - ...
;; - "z"

;; a Dictionary is a [List-of String], that is one of:
;; - '()
;; - (cons String Dictionary)
;; i.e. a list of words.

(define-struct lc [letter count] #:transparent)
;; an Letter-Count (short for Letter-Count) is a structure:
;;     (make-lc Letter PositiveNumber)
;; i.e. a piece of data describing how many times a
;; letter figures as the first letter in a Dictionary.

;;;; constants

(define DICTIONARY (read-lines "/usr/share/dict/words"))

;;;; functions

;; Dictionary -> Letter-Count
;; produces the Letter-Count for the letter that occurs most often
;; as the first one in the given Dictionary

(check-expect (most-frequent '()) '())
(check-expect
 (most-frequent '("a"))
 (make-lc "a" 1))
(check-expect
 (most-frequent '("a" "bc"))
 (make-lc "b" 1))
(check-expect
 (most-frequent '("a" "ab" "bc"))
 (make-lc "a" 2))
(check-expect
 (most-frequent '("a" "ab" "bc" "bcd" "bcdef" "c" "cd"))
 (make-lc "b" 3))
(check-expect
 (most-frequent '("kunam" "murphy" "zuko" "miguel" "miggy" "walter" "walthazarbobalthazarfeefiefofalthazar" "ringo"))
 (make-lc "m" 3))

(define (most-frequent a-dictionary)
  (local (;; [List-of Dictionary] -> [List-of Letter-Count]
          ;; returns a list of Letter-Counts from a list of Dictionaries
          (define (l-dictionary->l-lc l-dictionary)
            (map (lambda (dictionary) (make-lc (substring (car dictionary) 0 1) (length dictionary))) l-dictionary))
          ;; Letter-Count Letter-Count -> Boolean
          ;; checks whether lc1 has a larger lc-count than lc2
          (define (lc>=? lc1 lc2) (>= (lc-count lc1) (lc-count lc2))))
    (cond [(empty? a-dictionary) '()]
          [else (car (sort (l-dictionary->l-lc (words-by-first-letter.v4 a-dictionary)) lc>=?))])))

;; Dictionary -> [List-of Dictionary]
;; words-by-first-letter.v4 works just like words-by-first-letter
;; NOTE:
;; - has the best performance compared to other versions of words-by-first-letter
;; - original logic from Y.E.:
;;     https://gitlab.com/cs-study/htdp/-/blob/main/03-Abstraction/16-Using-Abstractions/Exercise-275.rkt?ref_type=heads

(check-expect (words-by-first-letter.v4 '()) '())
(check-expect (words-by-first-letter.v4 '("a")) '(("a")))
(check-expect (words-by-first-letter.v4 '("a" "b")) '(("a") ("b")))
(check-expect (words-by-first-letter.v4 '("a" "ab" "b")) '(("a" "ab") ("b")))
(check-expect
 (words-by-first-letter.v4 '("a" "ab" "abc" "bc" "bcd" "abcd" "cd" "cde" "abcde" "cdef"))
 '(("a" "ab" "abc" "abcd" "abcde")
   ("bc" "bcd")
   ("cd" "cde" "cdef")))
(check-expect
 (words-by-first-letter.v4 '("kunam" "murphy" "zuko" "miguel" "miggy" "walter" "walthazarbobalthazarfeefiefofalthazar" "ringo"))
 '(("kunam")
   ("miggy" "miguel" "murphy")
   ("ringo")
   ("walter" "walthazarbobalthazarfeefiefofalthazar")
   ("zuko")))

(define (words-by-first-letter.v4 a-dictionary)
  (local ((define sorted-dictionary
            (sort a-dictionary string<=?))
          ;; String [List-of Dictionary] -> [List-of Dictionary]
          ;; builds a Dictionary for words that start with the same letter
          ;; and conses it onto a given list of Dictionaries
          (define (dictionary-per-letter a-word l-dictionary)
            (cond [(empty? l-dictionary) (list (list a-word))]
                  [else (cond [(string=? (substring a-word 0 1) (substring (caar l-dictionary) 0 1))
                               (cons (cons a-word (car l-dictionary)) (cdr l-dictionary))]
                              [else (cons (list a-word) l-dictionary)])])))
    (foldr dictionary-per-letter '() sorted-dictionary)))

;;;; other versions of words-by-letter:

;; Dictionary -> [List-of Dictionary]
;; returns a list of Dictionaries, one per Letter

(check-expect (words-by-first-letter '()) '())
(check-expect (words-by-first-letter '("a")) '(("a")))
(check-expect (words-by-first-letter '("a" "b")) '(("a") ("b")))
(check-expect (words-by-first-letter '("a" "ab" "b")) '(("a" "ab") ("b")))
(check-expect
 (words-by-first-letter '("a" "ab" "abc" "bc" "bcd" "abcd" "cd" "cde" "abcde" "cdef"))
 '(("a" "ab" "abc" "abcd" "abcde")
   ("bc" "bcd")
   ("cd" "cde" "cdef")))
(check-expect
 (words-by-first-letter '("kunam" "murphy" "zuko" "miguel" "miggy" "walter" "walthazarbobalthazarfeefiefofalthazar" "ringo"))
 '(("kunam")
   ("miggy" "miguel" "murphy")
   ("ringo")
   ("walter" "walthazarbobalthazarfeefiefofalthazar")
   ("zuko")))

(define (words-by-first-letter a-dictionary)
  (local ((define sorted-dictionary (sort a-dictionary string<=?))
          ;; Dictionary -> [List-of Dictionary]
          ;; produces a list of Dictionaries, one per letter, in reverse order
          (define (dictionary-per-letter dictionary)
            (local (;; String [List-of Dictionary] -> [List-of Dictionary]
                    ;; adds a-word into a Dictionary if the first character
                    ;; of the former matches the first character of the first
                    ;; word of the latter, else creates a new Dictionary
                    (define (insert-word a-word l-dictionary)
                      (cond [(empty? l-dictionary) (list (list a-word))]
                            [else (cond [(string=? (substring a-word 0 1)
                                                   (substring (caar l-dictionary) 0 1))
                                         (cons (cons a-word (car l-dictionary)) (cdr l-dictionary))]
                                        [else (cons (car l-dictionary)
                                                    (insert-word a-word (cdr l-dictionary)))])])))
              (cond [(empty? dictionary) '()]
                    [else (insert-word (car dictionary) (dictionary-per-letter (cdr dictionary)))]))))
    (reverse (dictionary-per-letter sorted-dictionary))))

;; Dictionary -> [List-of Dictionary]
;; words-by-first-letter.v2 works just like words-by-first-letter

(check-expect (words-by-first-letter.v2 '()) '())
(check-expect (words-by-first-letter.v2 '("a")) '(("a")))
(check-expect (words-by-first-letter.v2 '("a" "b")) '(("a") ("b")))
(check-expect (words-by-first-letter.v2 '("a" "ab" "b")) '(("a" "ab") ("b")))
(check-expect
 (words-by-first-letter.v2 '("a" "ab" "abc" "bc" "bcd" "abcd" "cd" "cde" "abcde" "cdef"))
 '(("a" "ab" "abc" "abcd" "abcde")
   ("bc" "bcd")
   ("cd" "cde" "cdef")))
(check-expect
 (words-by-first-letter.v2 '("kunam" "murphy" "zuko" "miguel" "miggy" "walter" "walthazarbobalthazarfeefiefofalthazar" "ringo"))
 '(("kunam")
   ("miggy" "miguel" "murphy")
   ("ringo")
   ("walter" "walthazarbobalthazarfeefiefofalthazar")
   ("zuko")))

(define (words-by-first-letter.v2 a-dictionary)
  (local ((define sorted-dictionary
            (sort a-dictionary string<=?))
          (define available-letters
            (remove-duplicates
             (map (lambda (a-word) (substring a-word 0 1)) sorted-dictionary)))
          ;; Dictionary [List-of Letter] -> [List-of Dictionary]
          ;; returns a list of Dictionaries, one per Letter match in l-letters
          (define (dictionary-per-letter dictionary l-letters)
            (cond [(empty? l-letters) '()]
                  [else (local ((define dictionary-per-letter/cdr
                                  (dictionary-per-letter
                                   (filter
                                    (lambda (a-word)
                                      (not (string=? (car l-letters) (substring a-word 0 1))))
                                    dictionary)
                                   (cdr l-letters))))
                          (cons (filter
                                 (lambda (a-word)
                                   (string=? (car l-letters) (substring a-word 0 1)))
                                 dictionary)
                                dictionary-per-letter/cdr))])))
    (dictionary-per-letter sorted-dictionary available-letters)))

;; Dictionary -> [List-of Dictionary]
;; words-by-first-letter.v3 works just like words-by-first-letter

(check-expect (words-by-first-letter.v3 '()) '())
(check-expect (words-by-first-letter.v3 '("a")) '(("a")))
(check-expect (words-by-first-letter.v3 '("a" "b")) '(("a") ("b")))
(check-expect (words-by-first-letter.v3 '("a" "ab" "b")) '(("a" "ab") ("b")))
(check-expect
 (words-by-first-letter.v3 '("a" "ab" "abc" "bc" "bcd" "abcd" "cd" "cde" "abcde" "cdef"))
 '(("a" "ab" "abc" "abcd" "abcde")
   ("bc" "bcd")
   ("cd" "cde" "cdef")))
(check-expect
 (words-by-first-letter.v3 '("kunam" "murphy" "zuko" "miguel" "miggy" "walter" "walthazarbobalthazarfeefiefofalthazar" "ringo"))
 '(("kunam")
   ("miggy" "miguel" "murphy")
   ("ringo")
   ("walter" "walthazarbobalthazarfeefiefofalthazar")
   ("zuko")))

(define (words-by-first-letter.v3 a-dictionary)
  (local ((define sorted-dictionary
            (sort a-dictionary string<=?))
          (define available-letters
            (remove-duplicates
             (map
              (lambda (word-from-sorted-dictionary)
                (substring word-from-sorted-dictionary 0 1))
              sorted-dictionary)))
          ;; Letter -> Dictionary
          ;; extracts all words from sorted-dictionary that start
          ;; with a given Letter
          (define (dictionary-per-letter a-letter)
            (filter
             (lambda (word-from-sorted-dictionary)
               (string=? a-letter (substring word-from-sorted-dictionary 0 1)))
             sorted-dictionary)))
    (map dictionary-per-letter available-letters)))

;;;; application

(test)
