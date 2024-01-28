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

;; List-of-Tracks -> List-of-Strings
;; given a List-of-Tracks, retruns a list of all unique album
;; title as a List-of-Strings.

(check-expect (album-select-all-titles/unique list-of-tracks-example-0) '())
(check-expect (album-select-all-titles/unique list-of-tracks-example-1) (list "Down to Earth"))
(check-expect (album-select-all-titles/unique list-of-tracks-example-2)
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))
(check-expect (album-select-all-titles/unique list-of-tracks-example-3)
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))

(define (album-select-all-titles/unique list-of-tracks)
  (album-create-set (album-select-all-titles list-of-tracks)))

;; List-of-Strings -> List-of-Strings
;; removes duplicates of an album name in a List-of-Strings.

(check-expect (album-create-set (album-select-all-titles list-of-tracks-example-2))
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))
(check-expect (album-create-set (album-select-all-titles list-of-tracks-example-3))
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))

(define (album-create-set list-of-strings)
  (cond [(empty? list-of-strings) '()]
        [else (if (boolean? (member (first list-of-strings) (rest list-of-strings)))
                  (cons (first list-of-strings) (album-create-set (rest list-of-strings)))
                  (album-create-set (rest list-of-strings)))]))

;; List-of-Tracks -> List-of-Strings
;; given a List-of-Tracks, returns a list of its albums as a
;; List-of-Strings.

(check-expect (album-select-all-titles list-of-tracks-example-0) '())
(check-expect (album-select-all-titles list-of-tracks-example-1) (list "Down to Earth"))
(check-expect (album-select-all-titles list-of-tracks-example-2)
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))
(check-expect (album-select-all-titles list-of-tracks-example-3)
              (list "Down to Earth" "Down to Earth"
                    "Songs in the Key of Life" "Songs in the Key of Life"
                    "DAMN." "DAMN."))

(define (album-select-all-titles list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (cons (track-album (first list-of-tracks))
                    (album-select-all-titles (rest list-of-tracks)))]))

;;; application

(test)
