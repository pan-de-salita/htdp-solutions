#lang racket
(require test-engine/racket-tests)
(require 2htdp/batch-io)
(require 2htdp/itunes)

;;; data definitions

;; (define-struct date
;;   [year month day hour minutes second] #:transparent)
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

;; (define-struct track
;;   [name artist album time track# added play# played] #:transparent)
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
(define track-example-4
  (create-track "He Can Only Hold Her" "Amy Winehouse" "iTunes Festival: London 2007" 191866 7 (create-date 2014 10 20 14 31 25) 6 (create-date 2014 11 13 21 20 11)))

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
  (list (list track-example-2)
        (list track-example-1)
        (list track-example-0)))
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
;; from a List-of-Tracks, produces an LL-Tracks, one per album.
;; if there are any duplicate of tracks, they will be removed.

(check-expect (select-albums.v2 list-of-tracks-example-0) ll-tracks-example-0)
(check-expect (select-albums.v2 list-of-tracks-example-1) ll-tracks-example-1)
(check-expect (select-albums.v2 list-of-tracks-example-2) ll-tracks-example-2)
(check-expect (select-albums.v2 list-of-tracks-example-3) ll-tracks-example-2)
(check-expect (select-albums.v2 list-of-tracks-example-4)
              (list (list track-example-1 track-example-3)
                    (list track-example-2)
                    (list track-example-0)))

(define (select-albums.v2 list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (group-track/album.v2 (remove-duplicate-tracks list-of-tracks))]))

;; List-of-Tracks -> List-of-Tracks
;; removes duplicate Tracks from a List-of-Tracks

(check-expect (remove-duplicate-tracks list-of-tracks-example-0) '())
(check-expect (remove-duplicate-tracks list-of-tracks-example-1) list-of-tracks-example-1)
(check-expect (remove-duplicate-tracks list-of-tracks-example-3) list-of-tracks-example-2)

(define (remove-duplicate-tracks list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (if (boolean? (member (first list-of-tracks) (rest list-of-tracks)))
                  (cons (first list-of-tracks) (remove-duplicate-tracks (rest list-of-tracks)))
                  (remove-duplicate-tracks (rest list-of-tracks)))]))

;; List-of-Tracks -> LL-Tracks
;; from a List-of-Tracks, produces an LL-Tracks, one per album.

(check-expect (group-track/album.v2 list-of-tracks-example-0) ll-tracks-example-0)
(check-expect (group-track/album.v2 list-of-tracks-example-1) ll-tracks-example-1)
(check-expect (group-track/album.v2 list-of-tracks-example-2) ll-tracks-example-2)
(check-expect (group-track/album.v2 list-of-tracks-example-3)
              (list (list track-example-2 track-example-2)
                    (list track-example-1 track-example-1)
                    (list track-example-0 track-example-0)))
(check-expect (group-track/album.v2 list-of-tracks-example-4)
              (list (list track-example-1 track-example-3)
                    (list track-example-2)
                    (list track-example-0)))

(define (group-track/album.v2 list-of-tracks)
  (cond [(empty? list-of-tracks) '()]
        [else (insert-into-ll-track (first list-of-tracks) (group-track/album.v2 (rest list-of-tracks)))]))

;; Track LL-Tracks -> LL-Tracks
;; inserts a Tracks into an LL-Tracks depending on:
;; - if the Track has an album it belongs to in the given LL-Track,
;;   add said Track into corresponding list
;; - else, create a new list to accomodate the Track.

(check-expect (insert-into-ll-track track-example-3 ll-tracks-example-0) (list (list track-example-3)))
(check-expect (insert-into-ll-track track-example-3 ll-tracks-example-1) (list (list track-example-0) (list track-example-3)))
(check-expect (insert-into-ll-track track-example-3 ll-tracks-example-2)
              (list (list track-example-2)
                    (list track-example-3 track-example-1)
                    (list track-example-0)))

(define (insert-into-ll-track track ll-track)
  (cond [(empty? ll-track) (list (list track))]
        [else (if (string=? (track-album track) (track-album (first (first ll-track))))
                  (cons (cons track (first ll-track)) (rest ll-track))
                  (cons (first ll-track) (insert-into-ll-track track (rest ll-track))))]))

;;; application

(test)
(select-albums.v2 ITUNES-TRACKS)
