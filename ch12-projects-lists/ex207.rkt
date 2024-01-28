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

(define ITUNES-LOCATION "itunes.xml")
(define ITUNES-LISTS (read-itunes-as-lists ITUNES-LOCATION))

(define DEFAULT "no such association found.")

;;; function definitions

;; List-of-Association-Lists -> Number
;; computes the total amount of play time in a List-of-Association-Lists.

(check-expect (total-time/list ll-associations-example-0) 0)
(check-expect (total-time/list ll-associations-example-1) 162000)
(check-expect (total-time/list ll-associations-example-2) (+ 162000 428000 185000))
(check-expect (total-time/list ll-associations-example-3) (+ 162000 428000 185000 251000))

(define (total-time/list ll-associations)
  (cond [(empty? ll-associations) 0]
        [else (+ (second (assoc "Total Time" (first ll-associations)))
                 (total-time/list (rest ll-associations)))]))

;; NOTE: checking the total time with total-time/list produces different
;; results than with total-play-time/library (from ex.200). the former
;; function produces a higher total time. upon checking, read-itunes-as-tracks
;; produced a shorter list (with itunes.xml, length: 1667) than read-itunes-as-lists
;; (with same file, length: 1775).

;;; application

(test)
(total-time/list ITUNES-LISTS)
