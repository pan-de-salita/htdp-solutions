#lang racket

(require test-engine/racket-tests)

(struct address [first-name last-name street] #:transparent)
;; an Addr is a structure:
;;   (address String String String)
;; associates an address with a person's name
(define addr1 (address "robert" "findler" "south"))
(define addr2 (address "matthew" "flatt" "canyon"))
(define addr3 (address "shiriam" "krishna" "yellow"))

(define l-addr-test (list addr1 addr2 addr3))

;; [List-of Addr] -> String
;; creats a string from first names,
;; sorted in alphabetical order,
;; separated and surrounded by blank spaces

(check-expect (listing l-addr-test) " matthew robert shiriam ")

(define (listing l)
  (foldr string-append-with-space " " (sort (map address-first-name l) string<?)))

;; String String -> String
;; appends two strings, prefixes with " "

(check-expect (string-append-with-space "shiriam" " ") " shiriam ")

(define (string-append-with-space str1 str2)
  (string-append " " str1 str2))

(test)
