#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;; data definitions

;; a Word is one of:
;; - '()
;; - (cons 1String Word)
;; i.e. a Word is a list of 1Strings (letters)
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
;; - (cons Word '())
;; - (cons Word List-of-Words)
;; i.e. a list of Words.
;; examples:
(define LIST-OF-WORDS-EXAMPLE-0
  (list WORD-EXAMPLE-0))
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

(define DICTIONARY-LOCATION "/usr/share/dict/words")
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))

;;; functions

;; String -> List-of-Strings
;; finds all words that use the same letters as s.

;; (check-satisfied (alternative-words "cat") all-words-from-cat?)
;; (check-satisfied (alternative-words "rat") all-words-from-rat?)

;; (define (alternative-words a-string)
;;   (in-dictionary (words->strings (arrangements (string->word a-string)))))

;; Word -> List-of-Words
;; returns all rearrangements of the letters in a Word.

(check-expect (arrangements '()) (list '()))
(check-expect
 (arrangements (list "c" "a" "t"))
 (list (list "c" "a" "t")
       (list "t" "c" "a")
       (list "a" "t" "c")
       (list "c" "t" "a")
       (list "a" "c" "t")
       (list "t" "a" "c")))
(check-expect
 (arrangements (list "e" "e" "l"))
 (list (list "e" "e" "l") (list "l" "e" "e") (list "e" "l" "e")))
(check-expect
 (arrangements (list "l" "a" "n" "g"))
 (list (list "l" "a" "n" "g")
       (list "g" "l" "a" "n")
       (list "n" "g" "l" "a")
       (list "a" "n" "g" "l")
       (list "l" "g" "a" "n")
       (list "n" "l" "g" "a")
       (list "a" "n" "l" "g")
       (list "g" "a" "n" "l")
       (list "l" "n" "g" "a")
       (list "a" "l" "n" "g")
       (list "g" "a" "l" "n")
       (list "n" "g" "a" "l")
       (list "l" "a" "g" "n")
       (list "n" "l" "a" "g")
       (list "g" "n" "l" "a")
       (list "a" "g" "n" "l")
       (list "l" "n" "a" "g")
       (list "g" "l" "n" "a")
       (list "a" "g" "l" "n")
       (list "n" "a" "g" "l")
       (list "l" "g" "n" "a")
       (list "a" "l" "g" "n")
       (list "n" "a" "l" "g")
       (list "g" "n" "a" "l")))

(define (arrangements a-word)
  (if (empty? a-word)
      (list '())
      (insert-everywhere/in-all-words (first a-word) (arrangements (rest a-word)))))

;; 1String List-of-Words -> List-of-Words
;; inserts a letter into all Words at every position in a List-of-Words.

(check-expect (insert-everywhere/in-all-words "c" (list '()))
              (list (list "c")))
(check-expect (insert-everywhere/in-all-words "c" (list (list "a")))
              (list (list "c" "a") (list "a" "c")))
(check-expect (insert-everywhere/in-all-words "c" (list (list "a" "t")))
              (list (list "c" "a" "t")
                    (list "t" "c" "a")
                    (list "a" "t" "c")))
(check-satisfied
 (insert-everywhere/in-all-words "c" (list (list "a" "t") (list "t" "a")))
 all-words-of-cat?)

(define (insert-everywhere/in-all-words a-letter a-list-of-words)
  (if (empty? a-list-of-words)
      '()
      (append (insert-everywhere/in-one-word a-letter (first a-list-of-words) a-letter)
              (insert-everywhere/in-all-words a-letter (rest a-list-of-words)))))

;; 1String Word 1String -> List-of-Words
;; inserts a letter into a Word at every position.

(check-expect (insert-everywhere/in-one-word "c" '() "c") (list (list "c")))
(check-expect (insert-everywhere/in-one-word "c" (list "a") "c")
              (list (list "c" "a")
                    (list "a" "c")))
(check-expect (insert-everywhere/in-one-word "c" (list "a" "t") "c")
              (list (list "c" "a" "t")
                    (list "t" "c" "a")
                    (list "a" "t" "c")))

(define (insert-everywhere/in-one-word a-letter a-word initial-letter)
  (cond
    ;; deals with empty words:
    [(empty? a-word) (list (list a-letter))]
    ;; deals with one-letter-words:
    [(= (length a-word) 1) (insert-everywhere/in-one-letter-word a-letter a-word)]
    ;; deals with words with more than one letter:
    [(string=? (my-last a-word) initial-letter) (list (cons a-letter a-word))]
    [else (cons (cons a-letter a-word)
                (insert-everywhere/in-one-word (my-last a-word) (cons a-letter (but-last a-word)) initial-letter))]))

;; 1String Word -> List-of-Words
;; inserts a letter into a Word with only one element.

(check-expect (insert-everywhere/in-one-letter-word "c" (list "a"))
              (list (list "c" "a") (list "a" "c")))

(define (insert-everywhere/in-one-letter-word a-letter a-one-letter-word)
  (list (list a-letter (first a-one-letter-word))
        (list (first a-one-letter-word) a-letter)))

;; Non-empty-List -> Any
;; returns the last item on a non-empty list.

(check-expect (my-last (list "a" "b" "c")) "c")
(check-expect (my-last (list 1 2 3)) 3)
(check-expect (my-last (list #t #t #f)) #f)

(define (my-last a-non-empty-list)
  (first (reverse a-non-empty-list)))

;; Non-empty-List -> Any
;; returns all but the last element on non-empty list.

(check-expect (but-last (list "a" "b" "c")) (list "a" "b"))
(check-expect (but-last (list 1 2 3)) (list 1 2))
(check-expect (but-last (list #t #t #f)) (list #t #t))

(define (but-last a-non-empty-list)
  (reverse (rest (reverse a-non-empty-list))))

;; List-of-Words -> Boolean
;; checks if all elements on a List-of-Words belong to
;; all rearrangements of the letters in the word "cat".

(check-expect
 (all-words-of-cat?
  (list (list "c" "a" "t")
        (list "t" "c" "a")
        (list "a" "t" "c")
        (list "c" "t" "a")
        (list "a" "c" "t")
        (list "t" "a" "c")))
 #t)
(check-expect
 (all-words-of-cat?
 (list (list 1 2 3)
       (list "c" "a" "t")
       (list "t" "c" "a")
       (list "a" "t" "c")
       (list "c" "t" "a")
       (list "a" "c" "t")
       (list "t" "a" "c")))
 #f)

(define (all-words-of-cat? a-list-of-words)
  (if (empty? a-list-of-words)
      #t
      (and (my-member? (first a-list-of-words)
                    (list (list "c" "a" "t")
                          (list "t" "c" "a")
                          (list "a" "t" "c")
                          (list "c" "t" "a")
                          (list "a" "c" "t")
                          (list "t" "a" "c")))
           (all-words-of-cat? (rest a-list-of-words)))))

;; String List-of-Strings -> Boolean
;; checks if a String figures in a List-of-Strings.

(check-expect (my-member? "cat" (list "cat" "act")) #t)
(check-expect (my-member? "cat" (list "rat" "tar" "art")) #f)
(check-expect (my-member? "cat" '()) #f)

(define (my-member? a-string a-list-of-strings)
  (not (boolean? (member a-string a-list-of-strings))))

;;; application

(test)