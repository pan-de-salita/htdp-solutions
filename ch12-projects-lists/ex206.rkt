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

(define track-example-0
  (create-track "Hey Love" "Stevie Wonder" "Down to Earth" 162000 12 date-example-0 4 date-example-1))
(define track-example-1
  (create-track "As" "Stevie Wonder" "Songs in the Key of Life" 428000 16 date-example-2 15 date-example-3))
(define track-example-2
  (create-track "DNA" "Kendrick Lamar" "DAMN." 185000 2 date-example-4 2 date-example-5))
(define track-example-3
  (create-track "Ebony Eyes" "Stevie Wonder" "Songs in the Key of Life" 251000 19 date-example-6 20 date-example-7))

(define association-list-example-0
  (list (list "Name" "Hey Love")
        (list "Artist" "Stevie Wonder")
        (list "Album" "Down to Earth")
        (list "Total Time" 162000)
        (list "Track Number" 12)
        (list "Date Added" date-example-0)
        (list "Play Count" 4)
        (list "Play Date UTC" date-example-1)))
(define association-list-example-1
  (list (list "Name" "As")
        (list "Artist" "Stevie Wonder")
        (list "Album" "Songs in the Key of Life")
        (list "Total Time" 428000)
        (list "Track Number" 16)
        (list "Date Added" date-example-2)
        (list "Play Count" 15)
        (list "Play Date UTC" date-example-3)))
(define association-list-example-2
  (list (list "Name" "DNA")
        (list "Artist" "Kendrick Lamar")
        (list "Album" "DAMN.")
        (list "Total Time" 185000)
        (list "Track Number" 2)
        (list "Date Added" date-example-4)
        (list "Play Count" 2)
        (list "Play Date UTC" date-example-5)))
(define association-list-example-3
  (list (list "Name" "Ebony Eyes")
        (list "Artist" "Stevie Wonder")
        (list "Album" "Songs in the Key of Life")
        (list "Total Time" 251000)
        (list "Track Number" 19)
        (list "Date Added" date-example-6)
        (list "Play Count" 20)
        (list "Play Date UTC" date-example-7)))

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

(define ITUNES-LOCATION "itunes2.xml")
(define ITUNES-TRACKS (read-itunes-as-tracks ITUNES-LOCATION))
(define ITUNES-LISTS (read-itunes-as-lists ITUNES-LOCATION))

(define DEFAULT "no such association found.")

;;; function definitions

;; String Association-List Any -> Association/Any
;; returns the first Association whose first item is equal to
;; the given String key. returns Any default if there is no
;; such Association.

(check-expect (find-association "Name" association-list-example-3 DEFAULT) (list "Name" "Ebony Eyes"))
(check-expect (find-association "Artist" association-list-example-3 DEFAULT) (list "Artist" "Stevie Wonder"))
(check-expect (find-association "Album" association-list-example-3 DEFAULT) (list "Album" "Songs in the Key of Life"))
(check-expect (find-association "Total Time" association-list-example-3 DEFAULT) (list "Total Time" 251000))
(check-expect (find-association "Track Number" association-list-example-3 DEFAULT) (list "Track Number" 19))
(check-expect (find-association "Date Added" association-list-example-3 DEFAULT) (list "Date Added" date-example-6))
(check-expect (find-association "Play Count" association-list-example-3 DEFAULT) (list "Play Count" 20))
(check-expect (find-association "Play Date UTC" association-list-example-3 DEFAULT) (list "Play Date UTC" date-example-7))
(check-expect (find-association "Producer" association-list-example-3 DEFAULT) DEFAULT)

(define (find-association key association-list default)
  (cond [(empty? association-list) default]
        [else (if (string=? (first (first association-list)) key)
                  (first association-list)
                  (find-association key (rest association-list) default))]))

;;; application

(test)
