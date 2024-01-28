#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;;; constants and data definitions

;; a Letter is one of the following 1Strings:
;; - "a"
;; - ...
;; - "z"

;; a Word is one of:
;; - '()
;; - (cons Letter List-of-Letters)

;; a Dictionary is a List-of-Strings.
(define DICTIONARY-AS-LIST (read-lines "/usr/share/dict/words"))

;;;; functions

;; String -> List-of-Strings
;; returns the anagrams of the given String, including the given String.

(check-expect (anagrams "cat") (list "cat" "act"))
(check-expect (anagrams "food") (list "food"))

(define (anagrams a-string)
  (remove-duplicates (in-dictionary (words->strings (rearrangements (string->word a-string))))))

;; String -> Word
;; converts a String into a Word.

(check-expect (string->word "cat") (list "c" "a" "t"))
(check-expect (string->word "food") (list "f" "o" "o" "d"))

(define (string->word a-string)
  (cond [(= (string-length a-string) 0) '()]
        [else (cons (substring a-string 0 1) (string->word (substring a-string 1)))]))

;; Word -> List-of-Words
;; returns all possible configurations of a given Word.

(check-expect
 (rearrangements (list "c" "a" "t"))
 (insert-everywhere/all-words "c" (rearrangements (list "a" "t"))))
(check-expect
 (rearrangements (list "f" "o" "o" "d"))
 (insert-everywhere/all-words "f" (rearrangements (list "o" "o" "d"))))

(define (rearrangements a-word)
  (cond [(empty? a-word) (list '())]
        [else (insert-everywhere/all-words (car a-word) (rearrangements (cdr a-word)))]))

;; Letter List-of-Words -> List-of-Words
;; inserts the given Letter into all Words in the given List-of-Words
;; in all possible positions.

(check-expect
 (insert-everywhere/all-words "c" (list (list "a" "t") (list "t" "a")))
 (list (list "c" "a" "t") (list "t" "c" "a") (list "a" "t" "c") (list "c" "t" "a") (list "a" "c" "t") (list "t" "a" "c")))
(check-expect
 (insert-everywhere/all-words
  "f" (list (list "o" "o" "d") (list "d" "o" "o") (list "o" "d" "o")
            (list "o" "d" "o") (list "o" "o" "d") (list "d" "o" "o")))
 (list (list "f" "o" "o" "d") (list "d" "f" "o" "o") (list "o" "d" "f" "o") (list "o" "o" "d" "f")
       (list "f" "d" "o" "o") (list "o" "f" "d" "o") (list "o" "o" "f" "d") (list "d" "o" "o" "f")
       (list "f" "o" "d" "o") (list "o" "f" "o" "d") (list "d" "o" "f" "o") (list "o" "d" "o" "f")
       (list "f" "o" "d" "o") (list "o" "f" "o" "d") (list "d" "o" "f" "o") (list "o" "d" "o" "f")
       (list "f" "o" "o" "d") (list "d" "f" "o" "o") (list "o" "d" "f" "o") (list "o" "o" "d" "f")
       (list "f" "d" "o" "o") (list "o" "f" "d" "o") (list "o" "o" "f" "d") (list "d" "o" "o" "f")))

(define (insert-everywhere/all-words a-letter a-list-of-words)
  (cond [(empty? a-list-of-words) '()]
        [else (append (insert-everywhere a-letter (car a-list-of-words) (length (car a-list-of-words)))
                      (insert-everywhere/all-words a-letter (cdr a-list-of-words)))]))

;; Letter Word Number -> List-of-Words
;; inserts the given Letter into the given Word in all possible positions.

(check-expect
 (insert-everywhere "c" (list "a" "t") (length (list "a" "t")))
 (list (list "c" "a" "t") (list "t" "c" "a") (list "a" "t" "c")))
(check-expect
 (insert-everywhere "f" (list "o" "o" "d") (length (list "o" "o" "d")))
 (list (list "f" "o" "o" "d") (list "d" "f" "o" "o") (list "o" "d" "f" "o") (list "o" "o" "d" "f")))

(define (insert-everywhere a-prefix a-suffix counter)
  (cond [(= counter 0) (list (cons a-prefix a-suffix))]
        [else (cons (cons a-prefix a-suffix)
                    (insert-everywhere (car (reverse a-suffix))
                                       (cons a-prefix (reverse (cdr (reverse a-suffix))))
                                       (sub1 counter)))]))

;; List-of-Words -> List-of-Strings
;; converts a List-of-Words into a List-of-Strings.

(check-expect
 (words->strings
  (list (list "c" "a" "t") (list "t" "c" "a") (list "a" "t" "c") (list "c" "t" "a") (list "a" "c" "t") (list "t" "a" "c")))
 (list "cat" "tca" "atc" "cta" "act" "tac"))
(check-expect
 (words->strings
  (list (list "f" "o" "o" "d") (list "d" "f" "o" "o") (list "o" "d" "f" "o") (list "o" "o" "d" "f")
        (list "f" "d" "o" "o") (list "o" "f" "d" "o") (list "o" "o" "f" "d") (list "d" "o" "o" "f")
        (list "f" "o" "d" "o") (list "o" "f" "o" "d") (list "d" "o" "f" "o") (list "o" "d" "o" "f")
        (list "f" "o" "d" "o") (list "o" "f" "o" "d") (list "d" "o" "f" "o") (list "o" "d" "o" "f")
        (list "f" "o" "o" "d") (list "d" "f" "o" "o") (list "o" "d" "f" "o") (list "o" "o" "d" "f")
        (list "f" "d" "o" "o") (list "o" "f" "d" "o") (list "o" "o" "f" "d") (list "d" "o" "o" "f")))
 (list "food" "dfoo" "odfo" "oodf" "fdoo" "ofdo"
       "oofd" "doof" "fodo" "ofod" "dofo" "odof"
       "fodo" "ofod" "dofo" "odof" "food" "dfoo"
       "odfo" "oodf" "fdoo" "ofdo" "oofd" "doof"))

(define (words->strings a-list-of-words)
  (cond [(empty? a-list-of-words) '()]
        [else (cons (word->string (car a-list-of-words)) (words->strings (cdr a-list-of-words)))]))

;; Word -> String
;; converts a Word into a String.

(check-expect (word->string (list "c" "a" "t")) "cat")
(check-expect (word->string (list "f" "o" "o" "d")) "food")

(define (word->string a-word)
  (cond [(empty? a-word) ""]
        [else (string-append (car a-word) (word->string (cdr a-word)))]))

;; List-of-Strings -> List-of-Strings
;; retains all Strings from a List-of-Strings that figure in a Dictionary.

(check-expect
 (in-dictionary (words->strings (rearrangements (list "c" "a" "t"))))
 (list "cat" "act"))
(check-expect
 (in-dictionary (words->strings (rearrangements (list "f" "o" "o" "d"))))
 (list "food" "food"))

(define (in-dictionary a-list-of-strings)
  (cond [(empty? a-list-of-strings) '()]
        [else (cond [(not (boolean? (member (car a-list-of-strings) DICTIONARY-AS-LIST)))
                     (cons (car a-list-of-strings) (in-dictionary (cdr a-list-of-strings)))]
                    [else (in-dictionary (cdr a-list-of-strings))])]))

;;;; application

(test)
