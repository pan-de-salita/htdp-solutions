#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;;; data and constant definitions

;; a Letter is an alphabetical 1String:
;; - "a"
;; - ...
;; - "z"

;; a Word is one of:
;; - '()
;; - (cons Letter Word)
(define word-ex0 '())
(define word-ex1 (list "a" "b"))
(define word-ex2 (list "c" "a" "t"))
(define word-ex3 (list "i" "l" "l"))

;; a Dictionary is a [List-of Strings]
(define DICTIONARY (read-lines "/usr/share/dict/words"))

;;;; function definitions

;; solutions to exercise 259 ---------------------------------------------------

;; String -> [List-of String]
;; anagrams|local| words the same way as anagrams

(check-expect (anagrams-of|local| "") (anagrams-of ""))
(check-expect (anagrams-of|local| "cat") (anagrams-of "cat"))
(check-expect (anagrams-of|local| "ill") (anagrams-of "ill"))

(define (anagrams-of|local| a-string)
  (local (;; 1. convert a-string into a Word
          ;; String -> Word
          ;; converts a string into a Word
          (define string-converted-into-word
            (local ((define (string->word|local| a-string)
                      (cond [(string=? a-string "") '()]
                            [else (cons (substring a-string 0 1)
                                        (string->word|local| (substring a-string 1)))])))
              (string->word|local| a-string)))
          ;; 2. find all possible arrangements of the Word (previously a string)
          (define all-permutations (permutations-of|local| string-converted-into-word))
          ;; 3. convert permutations back into strings; remove duplicates
          ;; [List-of Word] -> [List-of String]
          ;; converts a list of Words into a list of strings
          (define permutations-converted-into-l-string
            (local ((define (l-word->l-string|local| l-word)
                      (local (;; Word -> String
                              ;; auxiliary function: converts a Word into a string
                              (define (word->string|local| a-word)
                                (cond [(empty? a-word) ""]
                                      [else (string-append (car a-word)
                                                           (word->string|local| (cdr a-word)))])))
                        (cond [(empty? l-word) '()]
                              [else (cons (word->string|local| (car l-word))
                                          (l-word->l-string|local| (cdr l-word)))]))))
              (remove-duplicates (l-word->l-string|local| all-permutations))))
          ;; 4. find all valid anagrams of original string, including itself
          (define DICTIONARY|LOCAL| (read-lines "/usr/share/dict/words"))
          ;; [List-of String] -> [List-of String]
          (define permutations-in-dictionary
            (local ((define (in-dictionary? a-string) (not (boolean? (member a-string DICTIONARY)))))
              (filter in-dictionary? permutations-converted-into-l-string))))
    permutations-in-dictionary))

;; Word -> [List-of Word]
;; permutations-of-local works the same way as permutations-of

(check-expect (permutations-of|local| '()) (permutations-of '()))
(check-expect (permutations-of|local| (list "c" "a" "t")) (permutations-of (list "c" "a" "t")))
(check-expect (permutations-of|local| (list "i" "l" "l")) (permutations-of (list "i" "l" "l")))

(define (permutations-of|local| a-word)
  (local (;; Letter [List-of Word] -> [List-of Word]
          ;; inserts a letter into all positions within every
          ;; Word in a list of Words
          (define (insert-everywhere/all-words|local| a-letter l-word)
            (cond [(empty? l-word) '()]
                  [else (append (insert-everywhere|local| a-letter (car l-word))
                                (insert-everywhere/all-words|local| a-letter (cdr l-word)))]))
          ;; Letter Word -> [List-of Word]
          ;; inserts a letter into all positions within a single Word
          (define (insert-everywhere|local| a-letter a-word)
            (cond [(empty? a-word) (list (list a-letter))]
                  [else (cons (cons a-letter a-word)
                              (prepend-all-words|local| (car a-word)
                                                       (insert-everywhere|local| a-letter (cdr a-word))))]))
          ;; Letter [List-of] -> [List-of Word]
          ;; prepends a letter onto all words within a list of Words
          (define (prepend-all-words|local| a-letter l-word)
            (cond [(empty? l-word) '()]
                  [else (cons (cons a-letter (car l-word))
                              (prepend-all-words|local| a-letter (cdr l-word)))])))
          (cond [(empty? a-word) (list '())]
                [else (insert-everywhere/all-words|local| (car a-word)
                                                          (permutations-of|local| (cdr a-word)))])))

;; original functions ----------------------------------------------------------

;; String -> [List-of String]
;; given a string, find all English words that are made up from
;; the same letters in given string

(check-expect (anagrams-of "") '())
(check-expect (anagrams-of "cat") (list "cat" "act"))
(check-expect (anagrams-of "ill") (list "ill"))

(define (anagrams-of a-string)
  (valid-words (remove-duplicate-strings (l-word->l-string (permutations-of (string->word a-string))))))

;; String -> Word
;; converts a string into a Word

(check-expect (string->word "") '())
(check-expect (string->word "ab") (list "a" "b"))
(check-expect (string->word "cat") (list "c" "a" "t"))
(check-expect (string->word "ill") (list "i" "l" "l"))

(define (string->word str)
  (cond [(string=? str "") '()]
        [else (cons (substring str 0 1)
                    (string->word (substring str 1)))]))

;; [List-of Word] -> [List-of String]
;; converts a list of words into a list of strings

(check-expect (l-word->l-string (list '())) (list ""))
(check-expect (l-word->l-string (list (list "a" "b" "c") (list "b" "a" "c") (list "b" "c" "a")))
              (list "abc" "bac" "bca"))

(define (l-word->l-string l-word)
  (cond [(empty? l-word) '()]
        [else (cons (word->string (car l-word))
                    (l-word->l-string (cdr l-word)))]))

;; Word -> String
;; converts a Word into a string

(check-expect (word->string '()) "")
(check-expect (word->string (list "c" "a" "t")) "cat")

(define (word->string a-word)
  (cond [(empty? a-word) ""]
        [else (string-append (car a-word) (word->string (cdr a-word)))]))

;; [List-of String] -> [List-of String]
;; removes duplicates of strings within a list of strings

(check-expect (remove-duplicate-strings '()) '())
(check-expect (remove-duplicate-strings (list "a" "b" "a" "c" "a")) (list "b" "c" "a"))

(define (remove-duplicate-strings l-string)
  (cond [(empty? l-string) '()]
        [else (cond [(not (boolean? (member (car l-string) (cdr l-string))))
                     (remove-duplicate-strings (cdr l-string))]
                    [else (cons (car l-string) (remove-duplicate-strings (cdr l-string)))])]))

;; [List-of String] -> [List-of String]
;; extracts only the strings that figure in DICTIONARY

(check-expect (valid-words '()) '())
(check-expect (valid-words
               (remove-duplicate-strings
                (l-word->l-string (list (list "c" "a" "t")
                                        (list "a" "c" "t")
                                        (list "a" "t" "c")
                                        (list "c" "t" "a")
                                        (list "t" "c" "a")
                                                                 (list "t" "a" "c")))))
              (list "cat" "act"))
(check-expect (valid-words
               (remove-duplicate-strings
                (l-word->l-string (list (list "i" "l" "l")
                                        (list "l" "i" "l")
                                        (list "l" "l" "i")
                                        (list "i" "l" "l")
                                        (list "l" "i" "l")
                                        (list "l" "l" "i")))))
              (list "ill"))

(define (valid-words l-string)
  (cond [(empty? l-string) '()]
        [else (cond [(not (boolean? (member (car l-string) DICTIONARY)))
                     (cons (car l-string) (valid-words (cdr l-string)))]
                    [else (valid-words (cdr l-string))])]))

;; Word -> [List-of Word]
;; returns a list of all possible permutations of a given Word

(check-expect (permutations-of '()) (list '()))
(check-expect (permutations-of (list "a" "b"))
              (list (list "a" "b")
                    (list "b" "a")))
(check-expect (permutations-of (list "c" "a" "t"))
              (list (list "c" "a" "t")
                    (list "a" "c" "t")
                    (list "a" "t" "c")
                    (list "c" "t" "a")
                    (list "t" "c" "a")
                    (list "t" "a" "c")))
(check-expect (permutations-of (list "i" "l" "l"))
              (list (list "i" "l" "l")
                    (list "l" "i" "l")
                    (list "l" "l" "i")
                    (list "i" "l" "l")
                    (list "l" "i" "l")
                    (list "l" "l" "i")))

(define (permutations-of a-word)
  (cond [(empty? a-word) (list '())]
        [else (insert-everywhere/all-words (car a-word) (permutations-of (cdr a-word)))]))

;; Letter [List-of Word] -> [List-of Word]
;; inserts a letter into all positions within every Word in a
;; list of Words

(check-expect (insert-everywhere/all-words "" '()) '())
(check-expect (insert-everywhere/all-words "a" (list (list "b")))
              (list (list "a" "b")
                    (list "b" "a")))
(check-expect (insert-everywhere/all-words "c" (list (list "a" "t") (list "t" "a")))
              (list (list "c" "a" "t")
                    (list "a" "c" "t")
                    (list "a" "t" "c")
                    (list "c" "t" "a")
                    (list "t" "c" "a")
                    (list "t" "a" "c")))
(check-expect (insert-everywhere/all-words "i" (list (list "l" "l") (list "l" "l")))
              (list (list "i" "l" "l")
                    (list "l" "i" "l")
                    (list "l" "l" "i")
                    (list "i" "l" "l")
                    (list "l" "i" "l")
                    (list "l" "l" "i")))

(define (insert-everywhere/all-words a-letter l-word)
  (cond [(empty? l-word) '()]
        [else (append (insert-everywhere a-letter (car l-word))
                      (insert-everywhere/all-words a-letter (cdr l-word)))]))

;; Letter Word -> [List-of Word]
;; inserts a letter into all positions within a single Word

(check-expect (insert-everywhere "" '()) (list (list "")))
(check-expect (insert-everywhere "a" (list "b"))
              (list (list "a" "b") (list "b" "a")))
(check-expect (insert-everywhere "c" (list "a" "t"))
              (list (list "c" "a" "t")
                    (list "a" "c" "t")
                    (list "a" "t" "c")))
(check-expect (insert-everywhere "i" (list "l" "l"))
              (list (list "i" "l" "l")
                    (list "l" "i" "l")
                    (list "l" "l" "i")))

(define (insert-everywhere a-letter a-word)
  (cond [(empty? a-word) (list (list a-letter))]
        [else (cons (cons a-letter a-word)
                    (prepend/all-words (car a-word)
                                       (insert-everywhere a-letter (cdr a-word))))]))

;; Letter [List-of Word] -> [List-of Word]
;; prepends a letter onto all words within a list of Words

(check-expect (prepend/all-words "" '()) '())
(check-expect (prepend/all-words "a" (list (list "b"))) (list (list "a" "b")))
(check-expect (prepend/all-words "a" (list (list "c" "t") (list "t" "c")))
              (list (list "a" "c" "t") (list "a" "t" "c")))

(define (prepend/all-words a-letter l-word)
  (cond [(empty? l-word) '()]
        [else (cons (cons a-letter (car l-word))
                    (prepend/all-words a-letter (cdr l-word)))]))

;;;; application

(test)
