#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)
(require 2htdp/itunes)

;;; data definitions

(define-struct date
  [year month day hour minutes second])
;; a Date is a structure:
;;     (make-date Number Number Number Number Number Number)
;; i.e. an instance records six pieces of information:
;; - year
;; - month [1,12]
;; - day [1, 31]
;; - hour [0, 23]
;; - minute [0, 59]
;; - second [0, 59]
(define date-example-0 (make-date 2023 6 8 3 14 15))
(define date-example-1 (make-date 2023 6 9 9 26 53))
(define date-example-2 (make-date 2023 6 13 5 9 3))
(define date-example-3 (make-date 2023 6 14 14 15 9))
(define date-example-4 (make-date 2023 6 20 2 6 5))
(define date-example-5 (make-date 2023 6 20 3 5 9))

(define-struct track
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
(define track-example-0
  (make-track "Hey Love" "Stevie Wonder" "Down to Earth" 162000 12 date-example-0 4 date-example-1))
(define track-example-1
  (make-track "As" "Stevie Wonder" "Songs in the Key of Life" 428000 16 date-example-2 15 date-example-3))
(define track-example-2
  (make-track "DNA" "Kendrick Lamar" "DAMN." 185000 2 date-example-4 2 date-example-5))

;; an List-of-Tracks is one of:
;; - '()
;; - (cons Track List-of-Tracks)
;; examples:
(define list-of-tracks-example-0 '())
(define list-of-tracks-example-1 (list track-example-0))
(define list-of-tracks-example-2 (list track-example-0 track-example-1 track-example-2))
(define list-of-tracks-example-3 (list track-example-0 track-example-0
                                       track-example-1 track-example-1
                                       track-example-2 track-example-2))

;; a List-of-Strings is one of:
;; - '()
;; - (cons String List-of-String)

;;; constants

(define ITUNES-LOCATION "itunes.xml")
;; (define ITUNES-TRACKS (read-itunes-as-tracks ITUNES-LOCATION))

;;; function definitions

;; String List-of-Tracks -> List-of-Strings
;; returns from a List-of-Tracks a new List-of-Tracks that belongs to
;; the given album.

(check-expect (tracks-select-from-album "Down to Earth" list-of-tracks-example-0) '())
(check-expect (tracks-select-from-album "Down to Earth" list-of-tracks-example-1) (list track-example-0))
(check-expect (tracks-select-from-album "Down to Earth" list-of-tracks-example-2) (list track-example-0))
(check-expect (tracks-select-from-album "Down to Earth" list-of-tracks-example-3) (list track-example-0 track-example-0))

(define (tracks-select-from-album album list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (if (same-album? (track-album (first list-of-tracks)) album)
                  (cons (first list-of-tracks) (tracks-select-from-album album (rest list-of-tracks)))
                  (tracks-select-from-album album (rest list-of-tracks)))]))

;; String String -> Boolean
;; checks if two album titles are the same.

(check-expect (same-album? "Down to Earth" "Down to Earth") #t)
(check-expect (same-album? "Down to Earth" "DAMN.") #f)

(define (same-album? album-1 album-2)
  (string=? album-1 album-2))

;;; application

(test)