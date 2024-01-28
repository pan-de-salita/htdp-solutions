#lang racket
(require test-engine/racket-tests)

(struct address [first-name last-name street] #:transparent)
;; an Addr is a structure:
;;   (address String String String)
;; associates an address with a person's name
(define l-addr-ex
  (list (address "Robert" "Findler" "South")
        (address "Matthew" "Flatt" "Canyon")
        (address "Shiriam" "Krishna" "Yellow")))

;; [List-of Addr] -> String
;; creates a string from first names,
;; sorted in alphabetical order,
;; separated and surrounded by blank spaces

(check-expect (listing l-addr-ex) " Matthew Robert Shiriam ")

(define (listing l-addr)
  (foldr string-append-with-space " "
         (sort (map address-first-name l-addr) string<?)))

;; String String -> String
;; appends two strings prefixed with " "

(check-expect (string-append-with-space "Matthew" " ") " Matthew ")

(define (string-append-with-space str1 str2)
  (string-append " " str1 str2))

;;;; listing function without built-in abstractions ----------------------------

;; [List-of Addr] -> String
;; produces the same output as the listing fuctions above

(check-expect (own-listing l-addr-ex) " Matthew Robert Shiriam ")

(define (own-listing l-addr)
  (l->str-with-space-at-end (sort-l (l-first-names l-addr) string<=?)))

;; [List-of Addr] -> [List-of String]
;; converts a list of Addr into a list of its first names

(check-expect (l-first-names l-addr-ex) (list "Robert" "Matthew" "Shiriam"))

(define (l-first-names l-addr)
  (cond [(empty? l-addr) '()]
        [else (cons (address-first-name (car l-addr))
                    (l-first-names (cdr l-addr)))]))

;; [X] [List-of X] [X X -> X] -> [List-of X]
;; sorts the contents of a list according to given comparator

(check-expect (sort-l '() string<?) '())
(check-expect (sort-l (l-first-names l-addr-ex) string<?) (list "Matthew" "Robert" "Shiriam"))

(define (sort-l l cmp)
  (cond [(empty? l) '()]
        [else (cond [(or (empty? (sort-l (cdr l) cmp))
                         (cmp (car l) (car (sort-l (cdr l) cmp))))
                     (cons (car l) (sort-l (cdr l) cmp))]
                    [else (sort-l (append (cdr l) (list (car l))) cmp)])]))

;; [List-of String] -> String
;; converts a list of strings into a single string
;; with a space before and after each element

(check-expect (l->str-with-space-at-end '()) " ")
(check-expect (l->str-with-space-at-end (sort-l (l-first-names l-addr-ex) string<=?)) " Matthew Robert Shiriam ")

(define (l->str-with-space-at-end l-str)
  (cond [(empty? l-str) " "]
        [else (string-append " " (car l-str)
                             (l->str-with-space-at-end (cdr l-str)))]))

(test)
