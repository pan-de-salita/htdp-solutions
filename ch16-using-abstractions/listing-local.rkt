#lang racket
(require test-engine/racket-tests)

(define-struct address [first-name last-name street] #:transparent)
;; An Addr is a structure:
;;   (make-address String String String)
;; interpretation associates an address with a person's name
(define l-addr-ex
  (list (make-address "Robert"   "Findler" "South")
        (make-address "Matthew"  "Flatt"   "Canyon")
        (make-address "Shiriam"  "Krishna" "Yellow")))

;; [List-of Addr] -> String
;; creates a string from first names,
;; sorted in alphabetical order,
;; separated and surrounded by blank spaces

(check-expect (listing l-addr-ex) " Matthew Robert Shiriam ")

(define (listing addr)
  (foldr string-append-with-space " "
         (sort (map address-first-name addr) string<=?)))

;; String String -> String
;; appends two strings
;; then prefixes with a space

(check-expect (string-append-with-space "Robert" " ") " Robert ")

(define (string-append-with-space str1 str2)
  (string-append " " str1 str2))

;;;; listing function with local definitions -----------------------------------

;; [List-of Addr] -> String
;; creates a string of first names,
;; sorted in alphabetical order,
;; separated and surrounded by blank spaces

(check-expect (listing-local l-addr-ex) (listing l-addr-ex))

(define (listing-local l-addr)
  (local (;; 1. extract first names
          (define first-names (map address-first-name l-addr))
          ;; 2. sort first names
          (define first-names-sorted (sort first-names string<=?))
          ;; 3. append them, add spaces
          ;; String String -> String
          ;; appends two strings, prefixes with " "
          (define (helper str1 str2)
            (string-append " " str1 str2))
          (define concat+spaces
            (foldr helper " " first-names-sorted)))
    concat+spaces))

(test)
