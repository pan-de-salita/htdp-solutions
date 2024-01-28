#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)
(require 2htdp/itunes)

;;; data definitions

(define-struct date-details
  [year month day hour minute second])
;; a Date is a structure:
;;     (make-date Number Number Number Number Number Number)
;; i.e. an instance records six pieces of information:
;; - year
;; - month [1,12]
;; - day [1, 31]
;; - hour [0, 23]
;; - minute [0, 59]
;; - second [0, 59]
(define date-details-example-0 (make-date-details 2023 6 8 3 14 15))
(define date-details-example-1 (make-date-details 2023 6 9 9 26 53))
(define date-details-example-2 (make-date-details 2023 6 13 5 9 3))
(define date-details-example-3 (make-date-details 2023 6 14 14 15 9))
(define date-details-example-4 (make-date-details 2023 6 20 2 6 5))
(define date-details-example-5 (make-date-details 2023 6 20 3 5 9))

(define-struct track-details
  [name artist album time track# added play# played])
;; a Track is a structure:
;;     (make-track String String String Number Number Date Number Date)
;; i.e. an instance records in order:
;; - the track's title
;; - it's artist
;; - to which album it belongs
;; - its playing time in milliseconds
;; - its position within the album
;; - the date it was added
;; - how often it has been played
;; - the date it was last played
(define track-details-example-0
  (make-track-details "Hey Love" "Stevie Wonder" "Down to Earth" 162000 12 date-details-example-0 4 date-details-example-1))
(define track-details-example-1
  (make-track-details "As" "Stevie Wonder" "Songs in the Key of Life" 428000 16 date-details-example-2 15 date-details-example-3))
(define track-details-example-2
  (make-track-details "DNA" "Kendrick Lamar" "DAMN." 185000 2 date-details-example-4 2 date-details-example-5))

;; an List-of-Tracks is one of:
;; - '()
;; - (cons Track List-of-Tracks)
;; examples:
(define list-of-tracks-example-0 '())
(define list-of-tracks-example-1 (list track-details-example-0))
(define list-of-tracks-example-2 (list track-details-example-0 track-details-example-1 track-details-example-2))
(define list-of-tracks-example-3
  (list track-details-example-0 track-details-example-0
        track-details-example-1 track-details-example-1
        track-details-example-2 track-details-example-2))

;; a List-of-Strings is one of:
;; - '()
;; - (cons String List-of-String)

;;; constants

(define MILLISECONDS/YEAR 31556926000)
(define MILLISECONDS/MONTH 2629800000)
(define MILLISECONDS/DAY 86400000)
(define MILLISECONDS/HOUR 3600000)
(define MILLISECONDS/MINUTE 60000)
(define MILLISECONDS/SECOND 1000)

(define ITUNES-LOCATION "itunes.xml")
;; (define ITUNES-TRACKS (read-itunes-as-tracks ITUNES-LOCATION))

;;; function definitions

;; String Date List-of-Tracks -> List-of-Tracks
;; from the given List-of-Tracks, returns a List-of-Tracks that
;; belong to the given album and have been played after the given date

(check-expect (tracks-select-by-album-and-date "Down to Earth" date-details-example-0 list-of-tracks-example-0) '())
(check-expect (tracks-select-by-album-and-date "Down to Earth" date-details-example-0 list-of-tracks-example-1)
              (list track-details-example-0))
(check-expect (tracks-select-by-album-and-date "Down to Earth" date-details-example-0 list-of-tracks-example-2)
              (list track-details-example-0))
(check-expect (tracks-select-by-album-and-date "Down to Earth" date-details-example-1 list-of-tracks-example-2)
              '())
(check-expect (tracks-select-by-album-and-date "Songs in the Key of Life" date-details-example-2 list-of-tracks-example-2)
              (list track-details-example-1))
(check-expect (tracks-select-by-album-and-date "Songs in the Key of Life" date-details-example-3 list-of-tracks-example-2)
              '())
(check-expect (tracks-select-by-album-and-date "DAMN." date-details-example-4 list-of-tracks-example-2)
              (list track-details-example-2))
(check-expect (tracks-select-by-album-and-date "DAMN." date-details-example-5 list-of-tracks-example-2)
              '())
(check-expect (tracks-select-by-album-and-date "Down to Earth" date-details-example-0 list-of-tracks-example-3)
              (list track-details-example-0 track-details-example-0))
(check-expect (tracks-select-by-album-and-date "Songs in the Key of Life" date-details-example-2 list-of-tracks-example-3)
              (list track-details-example-1 track-details-example-1))
(check-expect (tracks-select-by-album-and-date "DAMN." date-details-example-4 list-of-tracks-example-3)
              (list track-details-example-2 track-details-example-2))

(define (tracks-select-by-album-and-date album a-date list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (if (and (same-album? (track-details-album (first list-of-tracks)) album)
                       (occurs-after? (track-details-played (first list-of-tracks)) a-date))
                  (cons (first list-of-tracks) (tracks-select-by-album-and-date album a-date (rest list-of-tracks)))
                  (tracks-select-by-album-and-date album a-date (rest list-of-tracks)))]))

;; String String -> Boolean
;; checks if two album titles are the same.

(check-expect (same-album? "Down to Earth" "Down to Earth") #t)
(check-expect (same-album? "Down to Earth" "DAMN.") #f)

(define (same-album? album-1 album-2)
  (string=? album-1 album-2))

;; Date Date -> Boolean
;; checks if a Date-1 comes after Date-2.

(check-expect (occurs-after? date-details-example-1 date-details-example-0) #t)
(check-expect (occurs-after? date-details-example-0 date-details-example-1) #f)

(define (occurs-after? date-1 date-2)
  (> (date->milliseconds date-1) (date->milliseconds date-2)))

;; Date -> Number
;; converts a Date into its equivalent in milliseconds passed.

(define (date->milliseconds a-date)
  (+ (* (date-details-year a-date) MILLISECONDS/YEAR)
     (* (date-details-month a-date) MILLISECONDS/MONTH)
     (* (date-details-day a-date) MILLISECONDS/DAY)
     (* (date-details-hour a-date) MILLISECONDS/HOUR)
     (* (date-details-minute a-date) MILLISECONDS/MINUTE)
     (* (date-details-second a-date) MILLISECONDS/SECOND)))

;;; application

(test)
