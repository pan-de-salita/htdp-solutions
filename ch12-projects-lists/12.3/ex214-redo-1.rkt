#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;;; constants and data definitions ------------------------

;; a Letter is one of the following 1Strings:
;; - "a"
;; - ...
;; - "z"

;; a Word is one of:
;; - '()
;; - (cons Letter Word)
;; examples:
(define empty-word '())
(define at-as-word (list "a" "t"))
(define tac-as-word (list "t" "a" "c"))

;; a Dictionary is a List-of-Strings.
(define DICTIONARY-AS-LIST (read-lines "/usr/share/dict/words"))

;;;; functions  --------------------------------------------

;; String -> List-of-Strings
;; returns a list of anagrams of a given String, including the
;; given String itself.

(check-expect (anagrams "at") (list "at"))
(check-expect (anagrams "tac") (list "act" "cat"))
(check-expect (anagrams "book") (list "book"))

(define (anagrams a-string)
  (remove-duplicates (in-dictionary (words->strings (rearrangements (string->word a-string))))))

;; String -> Word
;; returns a given String as a Word.

(check-expect (string->word "") empty-word)
(check-expect (string->word "at") at-as-word)
(check-expect (string->word "tac") tac-as-word)

(define (string->word a-string)
  (cond [(string=? a-string "") '()]
        [else (cons (substring a-string 0 1) (string->word (substring a-string 1)))]))

;; List-of-Words -> List-of-Strings
;; returns a given List-of-Words as a List-of-Strings.

(check-expect (words->strings '()) '())
(check-expect (words->strings (list (list "a" "t") (list "t" "a"))) (list "at" "ta"))
(check-expect
 (words->strings
  (list (list "t" "a" "c") (list "a" "t" "c") (list "a" "c" "t")
        (list "t" "c" "a") (list "c" "t" "a") (list "c" "a" "t")))
 (list "tac" "atc" "act" "tca" "cta" "cat"))

(define (words->strings a-list-of-words)
  (cond [(empty? a-list-of-words) '()]
        [else (cons (word->string (car a-list-of-words)) (words->strings (cdr a-list-of-words)))]))

;; Word -> String
;; returns a given Word as a String.

(check-expect (word->string empty-word) "")
(check-expect (word->string at-as-word) "at")
(check-expect (word->string tac-as-word) "tac")

(define (word->string a-word)
  (cond [(empty? a-word) ""]
        [else (string-append (car a-word) (word->string (cdr a-word)))]))

;; List-of-Strings -> List-of-Strings
;; returns a given List-of-Strings with only the Strings that figure in DICTIONARY-AS-LIST.

(check-expect (in-dictionary '()) '())
(check-expect (in-dictionary (list "at" "ta")) (list "at"))
(check-expect (in-dictionary (list "tac" "atc" "act" "tca" "cta" "cat")) (list "act" "cat"))

(define (in-dictionary a-list-of-strings)
  (cond [(empty? a-list-of-strings) '()]
        [else (cond [(not (boolean? (member (car a-list-of-strings) DICTIONARY-AS-LIST)))
                     (cons (car a-list-of-strings) (in-dictionary (cdr a-list-of-strings)))]
                    [(in-dictionary (cdr a-list-of-strings))])]))

;; rearrangements ------------------------------------------
;; solution inspired by adaliu and S8A

;; Word -> List-of-Words
;; returns all the possible spelling rearrangements of a given Word.

(check-expect (rearrangements empty-word) (list '()))
(check-expect (rearrangements at-as-word) (list (list "a" "t") (list "t" "a")))
(check-expect
 (rearrangements tac-as-word)
 (list (list "t" "a" "c") (list "a" "t" "c") (list "a" "c" "t")
       (list "t" "c" "a") (list "c" "t" "a") (list "c" "a" "t")))

(define (rearrangements a-word)
  (cond
    ;; returns an empty Word as an empty List-of-Words.
    [(empty? a-word) (list '())]
    ;; inserts the first Letter of a-word into all possible positions of:
    [else (insert-everywhere/all-words (car a-word)
                                       ;; all possible spelling rearrangements of the rest of a-word.
                                       (rearrangements (cdr a-word)))]))

#|

NOTE to self:

let "a" be the Letter we intend to insert into the List-of-Words
(list (list "b" "c") (list "c" "b")), the possible spelling
rearrangements of the word (list "b" "c").

intended operation:

1. insert "a" into the first Word like so:
   a. (list "a" "b" "c")
   b. (list "b" "a" "c")
   c. (list "b" "c" "a")

2. insert "a" into the second Word like so:
   a. (list "a" "c" "b")
   b. (list "c" "a" "b")
   c. (list "c" "b" "a")

NOTE how as "a" is inserted into each of its succeeding positions,
the letters that follow it are moved behind, retraining their
original order:

   a. (list "a" [0] [1])
   b. (list [0] "a" [1])
   c. (list [0] [1] "a")

3. append all lists to arrive at:
   (list (list "a" "b" "c") (list "b" "a" "c") (list "b" "c" "a")
         (list "a" "c" "b") (list "c" "a" "b") (list "c" "b" "a"))

|#

;; Letter List-of-Words -> List-of-Words
;; inserts a given Letter into all possible positions of a given List-of-Words.

(check-expect (insert-everywhere/all-words "a" '()) '())
(check-expect (insert-everywhere/all-words "a" (list (list "b"))) (list (list "a" "b") (list "b" "a")))
(check-expect
 (insert-everywhere/all-words "a" (list (list "b" "c") (list "c" "b")))
 (list (list "a" "b" "c") (list "b" "a" "c") (list "b" "c" "a")
       (list "a" "c" "b") (list "c" "a" "b") (list "c" "b" "a")))

(define (insert-everywhere/all-words a-letter a-list-of-words)
  (cond
    ;; returns a empty list if a-list-of-words is empty
    [(empty? a-list-of-words) '()]
    ;; appends a List-of-Words with a-letter inserted in all possible positions into the first word of a-list-of-words to:
    [else (append (insert-everywhere a-letter (car a-list-of-words))
                  ;; a List-of-Words with a-letter inserted in all possible positions into the rest of a-list-of-words
                  (insert-everywhere/all-words a-letter (cdr a-list-of-words)))]))

;; Letter Word -> List-of-Words
;; inserts a given Letter into all possible positions of a given Word.

(check-expect (insert-everywhere "a" '()) (list (list "a")))
(check-expect (insert-everywhere "a" (list "b")) (list (list "a" "b") (list "b" "a")))
(check-expect
 (insert-everywhere "a" (list "b" "c"))
 (list (list "a" "b" "c") (list "b" "a" "c") (list "b" "c" "a")))
(check-expect
 (insert-everywhere "a" (list "b" "c" "d"))
 (list (list "a" "b" "c" "d") (list "b" "a" "c" "d") (list "b" "c" "a" "d") (list "b" "c" "d" "a")))

(define (insert-everywhere a-letter a-word)
  (cond
    ;; returns a List-of-Words with a-letter as the only Word
    ;; NOTE: this is because the List-of-Words with a-letter as
    ;; the only Word will be appended to the result of the
    ;; penultimate recursive call
    [(empty? a-word) (list (list a-letter))]
    ;; conses a List consisting of a-letter consed onto a-word to a List-of-Words consisting of:
    [else (cons (cons a-letter a-word)
                ;; the first letter of a-word prepended onto every Word in
                (prepend-everywhere (first a-word)
                                    ;; a List-of-Words with a-letter inserted in all possible positions of every Word within.
                                    (insert-everywhere a-letter (cdr a-word))))]))

;; Letter List-of-Words -> List-of-Words
;; prepends a given Letter onto every Word in a List-of-Words.

(check-expect (prepend-everywhere "a" '()) '())
(check-expect (prepend-everywhere "a" (list (list "b"))) (list (list "a" "b")))
(check-expect (prepend-everywhere "b" (list (list "a" "c") (list "c" "a"))) (list (list "b" "a" "c") (list "b" "c" "a")))

(define (prepend-everywhere a-letter a-list-of-words)
  (cond
    ;; returns a empty list if a-list-of-words is empty
    [(empty? a-list-of-words) '()]
    ;; conses the first Word in a-list-of-words with a letter prepended onto it onto
    [else (cons (cons a-letter (car a-list-of-words))
                ;; the rest of a-list-of-words with a-letter prepended onto every Word within
                (prepend-everywhere a-letter (cdr a-list-of-words)))]))

;;;; application  ------------------------------------------

(test)
