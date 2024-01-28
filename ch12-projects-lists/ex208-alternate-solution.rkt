#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)
(require 2htdp/itunes)

;;; test cases

(define date-example-0 (create-date 2023 6 8 3 14 15))
(define date-example-1 (create-date 2023 6 9 9 26 53))
(define date-example-2 (create-date 2023 6 13 5 9 3))
(define date-example-3 (create-date 2023 6 14 14 15 9))
(define date-example-4 (create-date 2023 6 20 2 6 5))
(define date-example-5 (create-date 2023 6 20 3 5 9))
(define date-example-6 (create-date 2023 7 1 11 5 20))
(define date-example-7 (create-date 2023 7 2 4 20 34))

(define association-list-example-0
  (list (list "Name" "Hey Love")
        (list "Artist" "Stevie Wonder")
        (list "Album" "Down to Earth")
        (list "Total Time" 162000)
        (list "Track Number" 12)
        (list "Date Added" date-example-0)
        (list "Play Count" 4)
        (list "Play Date UTC" date-example-1)
        (list "Favorite?" #t)))
(define association-list-example-1
  (list (list "Name" "As")
        (list "Artist" "Stevie Wonder")
        (list "Album" "Songs in the Key of Life")
        (list "Total Time" 428000)
        (list "Track Number" 16)
        (list "Date Added" date-example-2)
        (list "Play Count" 15)
        (list "Play Date UTC" date-example-3)
        (list "Favorite?" #f)))
(define association-list-example-2
  (list (list "Name" "DNA")
        (list "Artist" "Kendrick Lamar")
        (list "Album" "DAMN.")
        (list "Total Time" 185000)
        (list "Track Number" 2)
        (list "Date Added" date-example-4)
        (list "Play Count" 2)
        (list "Play Date UTC" date-example-5)
        (list "Favorite?" #t)
        (list "Live Version?" #f)))
(define association-list-example-3
  (list (list "Name" "Ebony Eyes")
        (list "Artist" "Stevie Wonder")
        (list "Album" "Songs in the Key of Life")
        (list "Total Time" 251000)
        (list "Track Number" 19)
        (list "Date Added" date-example-6)
        (list "Play Count" 20)
        (list "Play Date UTC" date-example-7)
        (list "Favorite?" #f)))

(define ll-associations-example-0 '())
(define ll-associations-example-1 (list association-list-example-0))
(define ll-associations-example-2
  (list association-list-example-0
        association-list-example-1
        association-list-example-2))
(define ll-associations-example-3
  (list association-list-example-0
        association-list-example-1
        association-list-example-2
        association-list-example-3))

;;; constants

(define ITUNES-LOCATION "itunes.xml")
(define ITUNES-LISTS (read-itunes-as-lists ITUNES-LOCATION))

(define DEFAULT "no such association found.")

;;; function definitions

;; List-of-Association-Lists -> List-of-List-of-Strings
;; from a List-of-Association-Lists, returns a List-of-Strings
;; that are associated with a Boolean attribute, with duplicates
;; removed.

(check-expect (boolean-attributes.v2 ll-associations-example-0) '())
(check-expect (boolean-attributes.v2 ll-associations-example-1) (list "Favorite?"))
(check-expect (boolean-attributes.v2 ll-associations-example-2) (list "Favorite?" "Live Version?"))
(check-expect (boolean-attributes.v2 ll-associations-example-3) (list "Favorite?" "Live Version?"))

(define (boolean-attributes.v2 ll-associations)
  (cond [(empty? ll-associations) '()]
        [else (remove-duplicates (my-append (collect-boolean-attributes (first ll-associations))
                                            (boolean-attributes.v2 (rest ll-associations))))]))

;; Association-List -> List-of-Strings
;; from an Association-List, returns a List-of-Strings that are
;; associated with a Boolean attribute.

(check-expect (collect-boolean-attributes association-list-example-0) (list "Favorite?"))
(check-expect (collect-boolean-attributes association-list-example-1) (list "Favorite?"))
(check-expect (collect-boolean-attributes association-list-example-2) (list "Favorite?" "Live Version?"))
(check-expect (collect-boolean-attributes association-list-example-3) (list "Favorite?"))

(define (collect-boolean-attributes association-list)
  (cond [(empty? association-list) '()]
        [else (if (boolean? (second (first association-list)))
                  (cons (first (first association-list)) (collect-boolean-attributes (rest association-list)))
                  (collect-boolean-attributes (rest association-list)))]))

;; None-Empty-List-of-Strings List-of-Strings -> List-of-Strings
;; appends List-of-Strings-1 to List-of-Strings-2.

(check-expect (my-append '() '()) '())
(check-expect (my-append (list "Some String") '()) (list "Some String"))
(check-expect (my-append (list "Some String") (list "Favorite?" "Live Version?")) (list "Some String" "Favorite?" "Live Version?"))

(define (my-append list-of-strings-1 list-of-strings-2)
  (cond [(empty? list-of-strings-1) list-of-strings-2]
        [else (cons (first list-of-strings-1) (my-append (rest list-of-strings-1) list-of-strings-2))]))

;;; application

(test)
(boolean-attributes.v2 ITUNES-LISTS)
