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

(define list-of-association-example-0
  '('("Name" "Hey Love")
    '("Artist" "Stevie Wonder")
    '("Album" "Down to Earth")
    '("Total Time" 162000)
    '("Track Number" 12)
    '("Date Added" date-example-0)
    '("Play Count" 4)
    '("Play Date UTC" date-example-1)))
(define list-of-association-example-1
  '('("Name" "As")
    '("Artist" "Stevie Wonder")
    '("Album" "Songs in the Key of Life")
    '("Total Time" 428000)
    '("Track Number" 16)
    '("Date Added" date-example-2)
    '("Play Count" 15)
    '("Play Date UTC" date-example-3)))
(define list-of-association-example-2
  '('("Name" "DNA")
    '("Artist" "Kendrick Lamar")
    '("Album" "DAMN.")
    '("Total Time" 185000)
    '("Track Number" 2)
    '("Date Added" date-example-4)
    '("Play Count" 2)
    '("Play Date UTC" date-example-5)))
(define list-of-association-example-3
  '('("Name" "Ebony Eyes")
    '("Artist" "Stevie Wonder")
    '("Album" "Songs in the Key of Life")
    '("Total Time" 251000)
    '("Track Number" 19)
    '("Date Added" date-example-6)
    '("Play Count" 20)
    '("Play Date UTC" date-example-7)))

(define ll-association-example-0 '())
(define ll-association-example-1 (list list-of-association-example-0))
(define ll-association-example-2
  (list list-of-association-example-0
        list-of-association-example-1
        list-of-association-example-2))
(define ll-association-example-3
  (list list-of-association-example-0
        list-of-association-example-1
        list-of-association-example-2
        list-of-association-example-3))

;;; constants

(define ITUNES-LOCATION "itunes2.xml")
(define ITUNES-TRACKS (read-itunes-as-tracks ITUNES-LOCATION))
(define ITUNES-LISTS (read-itunes-as-lists ITUNES-LOCATION))

;;; function definitions

;;; application

(test)
