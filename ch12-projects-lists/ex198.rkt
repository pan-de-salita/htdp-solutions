#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;;; data definitions

;; a Letter is one of the following 1Strings:
;; - "a"
;; - ...
;; - "z"
;; i.e. any character of the English alphabet.

;; a Dictionary is a List-of-Strings, that is one of:
;; - '()
;; - (cons String Dictionary)
;; i.e. a list of words.
;; examples:
(define dict-test0 '())
(define dict-test1 (list "a" "as"))
(define dict-test2 (list "a" "as" "b" "c" "cat" "code"))
(define dict-test3
  (list "bowl" "racket" "notebook"
        "pen" "pan" "keyboard" "water"
        "ball" "watch" "will"))

;; a List-of-Dictionaries is one of:
;; - '()
;; - (cons Dictionary List-of-Dictionaries)
;; i.e. a list of Dictionaries.

(define-struct lc [letter count])
;; an LC (short for Letter-Count) is a structure:
;;     (make-lc Letter PositiveNumber)
;; i.e. a piece of data describing how many times a
;; letter figures as the first letter in a Dictionary.
;; examples:
(define lc-test0 (make-lc "a" 2))
(define lc-test1 (make-lc "b" 1))
(define lc-test2 (make-lc "c" 3))

;; a List-of-LCs is one of:
;; - '()
;; - (cons LC List-of-LCs)
;; i.e. a list of Letter-Counts.
;; examples:
(define llc-test0 '())
(define llc-test1 (list lc-test0))
(define llc-test2 (list lc-test0 lc-test1 lc-test2))
(define llc-test3
  (list (make-lc "w" 3)
        (make-lc "b" 2)
        (make-lc "k" 1)
        (make-lc "p" 2)
        (make-lc "n" 1)
        (make-lc "r" 1)))

;;; constants

(define LOCATION "/usr/share/dict/words")
(define DICTIONARY (read-lines LOCATION))

;;; functions

;; Dictionary -> LC
;; returns the LC for the letter that occurs most often as
;; the first letter in the given dictionary.

(check-expect (most-frequent.v2 dict-test0) (most-frequent dict-test0))
(check-expect (most-frequent.v2 dict-test1) (most-frequent dict-test1))
(check-expect (most-frequent.v2 dict-test2) (most-frequent dict-test2))
(check-expect (most-frequent.v2 dict-test3) (most-frequent dict-test3))
(check-expect (most-frequent.v2 DICTIONARY) (most-frequent DICTIONARY))

(define (most-frequent.v2 dict)
  (if (empty? dict)
      '()
      (largest-lc (count-by-letter.v2 (words-by-first-letter dict)))))

;; Dictionary -> List-of-Dictionaries
;; produces a List-of-Dictionaries, one per Letter.

(check-expect (words-by-first-letter dict-test0) '())
(check-expect (words-by-first-letter dict-test1) (list dict-test1))
(check-expect
 (words-by-first-letter dict-test2)
 (list (list "c" "cat" "code")
       (list "b")
       (list "a" "as")))
(check-expect
 (words-by-first-letter dict-test3)
 (list (list "water" "watch" "will")
       (list "bowl" "ball")
       (list "keyboard")
       (list "pen" "pan")
       (list "notebook")
       (list "racket")))

(define (words-by-first-letter dict)
  (cond [(empty? dict) '()]
        [else (include-to-ldict (first dict)
                                (words-by-first-letter (rest dict)))]))

;; String List-of-Dictionaries -> List-of-Dictionaries
;; modifies a given List-of-Dictionaries ldict depending on:
;; - if the String s starts with a letter that doesn't have its
;;   own Dictionary, create a new Dictionary to accomodate s
;; - if the String s starts with a letter that does have its own
;;   Dictionary, add it to the appropriate Dictionary

(check-expect (include-to-ldict "abstract" '()) (list (list "abstract")))
(check-expect (include-to-ldict "abstract" (list dict-test1)) (list (cons "abstract" dict-test1)))
(check-expect (include-to-ldict "machine" (list dict-test1)) (list dict-test1 (list "machine")))
(check-expect
 (include-to-ldict
  "machine"
  (list (list "a" "as")
        (list "b")
        (list "c" "cat" "code")))
 (list (list "a" "as")
       (list "b")
       (list "c" "cat" "code")
       (list "machine")))

(define (include-to-ldict s ldict)
  (cond [(empty? ldict) (list (cons s '()))]
        [else (if (string=? (substring s 0 1) (substring (first (first ldict)) 0 1))
                  (cons (cons s (first ldict)) (rest ldict))
                  (cons (first ldict) (include-to-ldict s (rest ldict))))]))

;; List-of-Dictionaries -> List-of-LCs
;; counts how often each letter figures as the first letter
;; of a word in a given List-of-Dictionaries ldict.

(check-expect (count-by-letter.v2 (words-by-first-letter dict-test0)) llc-test0)
(check-expect (count-by-letter.v2 (words-by-first-letter dict-test1)) llc-test1)
(check-expect (count-by-letter.v2 (words-by-first-letter dict-test2)) (reverse llc-test2))
(check-expect (count-by-letter.v2 (words-by-first-letter dict-test3)) llc-test3)

(define (count-by-letter.v2 ldict)
  (cond [(empty? ldict) '()]
        [else (cons (make-lc (substring (first (first ldict)) 0 1) (lc-counter (first ldict)))
                    (count-by-letter.v2 (rest ldict)))]))

;; Dictionary -> LC
;; returns the LC for the letter that occurs most often as
;; the first letter in the given dictionary.

(check-expect (most-frequent dict-test0) '())
(check-expect (most-frequent dict-test1) (make-lc "a" 2))
(check-expect (most-frequent dict-test2) (make-lc "c" 3))
(check-expect (most-frequent dict-test3) (make-lc "w" 3))

(define (most-frequent dict)
  (if (empty? dict)
      '()
      (largest-lc (count-by-letter dict))))

;; NE-List-of-LCs -> LC
;; returns the LC with the highest lc-count in a List-of-LCs.

(check-expect (largest-lc llc-test1) lc-test0)
(check-expect (largest-lc llc-test2) lc-test2)
(check-expect (largest-lc llc-test3) (make-lc "w" 3))

(define (largest-lc llc)
  (cond [(empty? (rest llc)) (first llc)]
        [else (if (>= (lc-count (first llc)) (lc-count (second llc)))
                  (largest-lc (cons (first llc) (rest (rest llc))))
                  (largest-lc (rest llc)))]))

;; NE-Dictionary -> LC
;; creates an lc-count given a non-empty Dictionary.

(check-expect (lc-counter dict-test0) 0)
(check-expect (lc-counter dict-test1) 2)

(define (lc-counter dict)
  (cond [(empty? dict) 0]
        [else (+ 1 (lc-counter (rest dict)))]))

;; Dictionary -> List-of-LCs
;; counts how often each letter figures as the first letter
;; of a word in a given Dictionary d.

(check-expect (count-by-letter dict-test0) llc-test0)
(check-expect (count-by-letter dict-test1) llc-test1)
(check-expect (count-by-letter dict-test2) (reverse llc-test2))
(check-expect (count-by-letter dict-test3) llc-test3)

(define (count-by-letter dict)
  (cond [(empty? dict) '()]
        [else (llc-maker (substring (first dict) 0 1)
                         (count-by-letter (rest dict)))]))

;; Letter List-of-LCs -> List-of-LCs
;; modifies a given List-of-LCs llc depending on:
;; - if the Letter figures as an LC, increment its count by 1
;; - if a given Letter doesn't figure as an LC, add it

(check-expect (llc-maker "a" llc-test0) (list (make-lc "a" 1)))
(check-expect (llc-maker "a" llc-test1) (list (make-lc "a" 3)))
(check-expect (llc-maker "a" llc-test2) (list (make-lc "a" 3) (make-lc "b" 1) (make-lc "c" 3)))
(check-expect (llc-maker "d" llc-test2)
              (list (make-lc "a" 2)
                    (make-lc "b" 1)
                    (make-lc "c" 3)
                    (make-lc "d" 1)))
(check-expect (llc-maker "f" llc-test3)
              (list (make-lc "w" 3)
                    (make-lc "b" 2)
                    (make-lc "k" 1)
                    (make-lc "p" 2)
                    (make-lc "n" 1)
                    (make-lc "r" 1)
                    (make-lc "f" 1)))

(define (llc-maker letter llc)
  (cond [(empty? llc) (cons (make-lc letter 1) '())]
        [else (if (not (string=? letter (lc-letter (first llc))))
                  (cons (first llc) (llc-maker letter (rest llc)))
                  (cons (make-lc letter (add1 (lc-count (first llc)))) (rest llc)))]))

;;; application

(test)
