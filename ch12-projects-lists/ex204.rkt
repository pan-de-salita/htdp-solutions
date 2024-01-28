#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)
(require 2htdp/itunes)

;;; data definitions

;; (define-struct date
;;  [year month day hour minutes second] #:transparent)
;; a Date is a structure:
;;     (make-date Number Number Number Number Number Number)
;; i.e. an instance records six pieces of information:
;; - year
;; - month [1,12]
;; - day [1, 31]
;; - hour [0, 23]
;; - minute [0, 59]
;; - second [0, 59]
(define date-example-0 (create-date 2023 6 8 3 14 15))
(define date-example-1 (create-date 2023 6 9 9 26 53))
(define date-example-2 (create-date 2023 6 13 5 9 3))
(define date-example-3 (create-date 2023 6 14 14 15 9))
(define date-example-4 (create-date 2023 6 20 2 6 5))
(define date-example-5 (create-date 2023 6 20 3 5 9))
(define date-example-6 (create-date 2023 7 1 11 5 20))
(define date-example-7 (create-date 2023 7 2 4 20 34))

;;(define-struct track
;;  [name artist album time track# added play# played] #:transparent)
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
  (create-track "Hey Love" "Stevie Wonder" "Down to Earth" 162000 12 date-example-0 4 date-example-1))
(define track-example-1
  (create-track "As" "Stevie Wonder" "Songs in the Key of Life" 428000 16 date-example-2 15 date-example-3))
(define track-example-2
  (create-track "DNA" "Kendrick Lamar" "DAMN." 185000 2 date-example-4 2 date-example-5))
(define track-example-3
  (create-track "Ebony Eyes" "Stevie Wonder" "Songs in the Key of Life" 251000 19 date-example-6 20 date-example-7))

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
(define list-of-tracks-example-4 (list track-example-0 track-example-1 track-example-2 track-example-3))

;; a List-of-List-of-Tracks (LL-Tracks for short) is one of:
;; - '()
;; - (cons List-of-Tracks List-of-List-of-Tracks)
;; examples:
(define ll-tracks-example-0 '())
(define ll-tracks-example-1 (list list-of-tracks-example-1))
(define ll-tracks-example-2
  (list (list track-example-0)
        (list track-example-1)
        (list track-example-2)))
(define ll-tracks-example-3
  (list (list track-example-0)
        (list track-example-2)
        (list track-example-1 track-example-3)))

;; a List-of-Strings is one of:
;; - '()
;; - (cons String List-of-String)

;;; constants

(define ITUNES-LOCATION "itunes.xml")
(define ITUNES-TRACKS (read-itunes-as-tracks ITUNES-LOCATION))

;;; function definitions

;; List-of-Tracks -> LL-Tracks
;; produces an LL-Tracks from a List-of-Tracks, one per album.
;; unlike select-albums below, this function removes all duplicate
;; Tracks.

(check-expect (select-albums/unique list-of-tracks-example-0) ll-tracks-example-0)
(check-expect (select-albums/unique list-of-tracks-example-1) ll-tracks-example-1)
(check-expect (select-albums/unique list-of-tracks-example-2) ll-tracks-example-2)
(check-expect (select-albums/unique list-of-tracks-example-3) ll-tracks-example-2)
(check-expect (select-albums/unique list-of-tracks-example-4) ll-tracks-example-3)

(define (select-albums/unique list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (group-tracks/album (select-all-album-titles/unique list-of-tracks) (remove-duplicate-tracks list-of-tracks))]))

;; List-of-Tracks -> List-of-Tracks
;; removes duplicate Tracks from a List-of-Tracks.

(check-expect (remove-duplicate-tracks list-of-tracks-example-0) '())
(check-expect (remove-duplicate-tracks list-of-tracks-example-1) list-of-tracks-example-1)
(check-expect (remove-duplicate-tracks list-of-tracks-example-3) list-of-tracks-example-2)
(check-expect (remove-duplicate-tracks list-of-tracks-example-4) list-of-tracks-example-4)

(define (remove-duplicate-tracks list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (if (boolean? (member (first list-of-tracks) (rest list-of-tracks)))
                  (cons (first list-of-tracks) (remove-duplicate-tracks (rest list-of-tracks)))
                  (remove-duplicate-tracks (rest list-of-tracks)))]))

;; List-of-Tracks -> LL-Tracks
;; produces an LL-Tracks from a List-of-Tracks, one per album.

(check-expect (select-albums list-of-tracks-example-0) ll-tracks-example-0)
(check-expect (select-albums list-of-tracks-example-1) ll-tracks-example-1)
(check-expect (select-albums list-of-tracks-example-2) ll-tracks-example-2)
(check-expect (select-albums list-of-tracks-example-3)
              (list (list track-example-0 track-example-0)
                    (list track-example-1 track-example-1)
                    (list track-example-2 track-example-2)))
(check-expect (select-albums list-of-tracks-example-4) ll-tracks-example-3)

(define (select-albums list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (group-tracks/album (select-all-album-titles/unique list-of-tracks) list-of-tracks)]))

;; List-of-Strings List-of-Tracks -> LL-Tracks
;; constructs an LL-Tracks according to given list of albums.

(check-expect
 (group-tracks/album
  (select-all-album-titles/unique list-of-tracks-example-0)
  list-of-tracks-example-0)
 '())
(check-expect
 (group-tracks/album
  '()
  list-of-tracks-example-0)
 '())
(check-expect
 (group-tracks/album
  (select-all-album-titles/unique list-of-tracks-example-1)
  list-of-tracks-example-1)
 ll-tracks-example-1)
(check-expect
 (group-tracks/album
  (select-all-album-titles/unique list-of-tracks-example-2)
  list-of-tracks-example-2)
 ll-tracks-example-2)
(check-expect
 (group-tracks/album
  (select-all-album-titles/unique list-of-tracks-example-3)
  list-of-tracks-example-3)
 (list (list track-example-0 track-example-0)
       (list track-example-1 track-example-1)
       (list track-example-2 track-example-2)))
(check-expect
 (group-tracks/album
  (select-all-album-titles/unique list-of-tracks-example-4)
  list-of-tracks-example-4)
 ll-tracks-example-3)

(define (group-tracks/album list-of-albums list-of-tracks)
  (cond [(or (empty? list-of-albums) (empty? list-of-tracks)) '()]
        [else (cons (select-tracks-from-album (first list-of-albums) list-of-tracks)
                    (group-tracks/album (rest list-of-albums) list-of-tracks))]))

;; String List-of-Tracks -> List-of-Tracks
;; returns from a List-of-Tracks a new List-of-Tracks that belongs to
;; the given album.

(check-expect (select-tracks-from-album "Down to Earth" list-of-tracks-example-0) '())
(check-expect (select-tracks-from-album "Down to Earth" list-of-tracks-example-1) (list track-example-0))
(check-expect (select-tracks-from-album "Down to Earth" list-of-tracks-example-2) (list track-example-0))
(check-expect (select-tracks-from-album "Down to Earth" list-of-tracks-example-3) (list track-example-0 track-example-0))

(define (select-tracks-from-album album list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (if (album=? (track-album (first list-of-tracks)) album)
                  (cons (first list-of-tracks) (select-tracks-from-album album (rest list-of-tracks)))
                  (select-tracks-from-album album (rest list-of-tracks)))]))

;; String String -> Boolean
;; checks if the two album titles are equal.

(check-expect (album=? "DAMN." "DAMN.") #t)
(check-expect (album=? "DAMN." "Down to Earth") #f)

(define (album=? album-title-1 album-title-2)
  (string=? album-title-1 album-title-2))

;; List-of-Tracks -> List-of-Strings
;; given a List-of-Tracks, retruns a list of all unique album
;; title as a List-of-Strings.

(check-expect (select-all-album-titles/unique list-of-tracks-example-0) '())
(check-expect (select-all-album-titles/unique list-of-tracks-example-1) (list "Down to Earth"))
(check-expect (select-all-album-titles/unique list-of-tracks-example-2)
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))
(check-expect (select-all-album-titles/unique list-of-tracks-example-3)
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))

(define (select-all-album-titles/unique list-of-tracks)
  (create-album-set (select-all-album-titles list-of-tracks)))

;; List-of-Strings -> List-of-Strings
;; removes duplicates of an album name in a List-of-Strings.
;; NOTE: albums with duplicates are inserted into the back
;; of the resulting List-of-Strings. check last test case.

(check-expect (create-album-set (select-all-album-titles list-of-tracks-example-2))
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))
(check-expect (create-album-set (select-all-album-titles list-of-tracks-example-3))
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))
(check-expect (create-album-set (select-all-album-titles list-of-tracks-example-4))
              (list "Down to Earth" "DAMN." "Songs in the Key of Life"))

(define (create-album-set list-of-strings)
  (cond [(empty? list-of-strings) '()]
        [else (if (boolean? (member (first list-of-strings) (rest list-of-strings)))
                  (cons (first list-of-strings) (create-album-set (rest list-of-strings)))
                  (create-album-set (rest list-of-strings)))]))

;; List-of-Tracks -> List-of-Strings
;; given a List-of-Tracks, returns a list of its albums as a
;; List-of-Strings.

(check-expect (select-all-album-titles list-of-tracks-example-0) '())
(check-expect (select-all-album-titles list-of-tracks-example-1) (list "Down to Earth"))
(check-expect (select-all-album-titles list-of-tracks-example-2)
              (list "Down to Earth" "Songs in the Key of Life" "DAMN."))
(check-expect (select-all-album-titles list-of-tracks-example-3)
              (list "Down to Earth" "Down to Earth"
                    "Songs in the Key of Life" "Songs in the Key of Life"
                    "DAMN." "DAMN."))

(define (select-all-album-titles list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (cons (track-album (first list-of-tracks)) (select-all-album-titles (rest list-of-tracks)))]))

;;; application

(test)
(select-albums ITUNES-TRACKS)
