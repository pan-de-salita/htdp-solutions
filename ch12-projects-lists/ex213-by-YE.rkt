#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;; all credit goes to Y.E.
;; link to solution: https://gitlab.com/cs-study/htdp/-/blob/main/02-Arbitrarily-Large-Data/12-Projects-Lists/Exercise-213.rkt

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

;; Word -> List-of-Words
;; returns all rearrangements of the letters in a Word.

(check-expect (arrangements '()) (list '()))
(check-expect
 (arrangements (list "c" "a" "t"))
 (list (list "c" "a" "t")
       (list "a" "c" "t")
       (list "a" "t" "c")
       (list "c" "t" "a")
       (list "t" "c" "a")
       (list "t" "a" "c")))

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
                    (list "a" "c" "t")
                    (list "a" "t" "c")))
(check-satisfied
 (insert-everywhere/in-all-words "c" (list (list "a" "t") (list "t" "a")))
 all-words-of-cat?)

(define (insert-everywhere/in-all-words a-letter a-list-of-words)
  (if (empty? a-list-of-words)
      '()
      (append (insert-everywhere/in-one-word a-letter '() (first a-list-of-words))
              (insert-everywhere/in-all-words a-letter (rest a-list-of-words)))))

;; 1String Word Word -> List-of-Words
;; inserts a letter into a Word at every position.

(check-expect (insert-everywhere/in-one-word "c" '() '()) (list (list "c")))
(check-expect (insert-everywhere/in-one-word "c" '() (list "a"))
              (list (list "c" "a")
                    (list "a" "c")))
(check-expect (insert-everywhere/in-one-word "c" '() (list "a" "t"))
              (list (list "c" "a" "t")
                    (list "a" "c" "t")
                    (list "a" "t" "c")))

(define (insert-everywhere/in-one-word a-letter a-prefix a-suffix)
  (if (empty? a-suffix)
      (list (append a-prefix (list a-letter)))
      (append (list (append a-prefix (list a-letter) a-suffix))
              (insert-everywhere/in-one-word a-letter
                                             (append a-prefix (list (first a-suffix)))
                                             (rest a-suffix)))))

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

;; all credit goes to Y.E.
;; link to solution: https://gitlab.com/cs-study/htdp/-/blob/main/02-Arbitrarily-Large-Data/12-Projects-Lists/Exercise-213.rkt
