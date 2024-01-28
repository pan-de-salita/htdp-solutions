#lang racket
(require test-engine/racket-tests)
(require racket/date)

;;;; data definitions

(define date-example-0 (date* 1 36 22 10 8 2020 3 234 #f 28800 834644794 "PST"))
(define date-example-1 (date* 2 36 22 23 8 2020 3 234 #f 28800 834644794 "PST"))
(define date-example-2 (date* 3 36 22 03 9 2021 3 234 #f 28800 834644794 "PST"))
(define date-example-3 (date* 4 36 22 23 10 2021 3 234 #f 28800 834644794 "PST"))
(define date-example-4 (date* 5 36 22 24 3 2022 3 234 #f 28800 834644794 "PST"))
(define date-example-5 (date* 6 36 22 23 8 2022 3 234 #f 28800 834644794 "PST"))
(define date-example-6 (date* 7 36 22 10 8 2023 3 234 #f 28800 834644794 "PST"))
(define date-example-7 (date* 8 36 22 23 8 2023 3 234 #f 28800 834644794 "PST"))

(struct track [name artist album duration track# added play# played] #:transparent)
;; a Track is a structure:
;;   (track String String String Number Number Date Number Date)
;; An instance records in order: the track's
;; title, its producing artist, to which album it belongs,
;; its playing time in milliseconds, its position within the
;; album, the date it was added, how often it has been
;; played, and the date when it was last played

(define track-example-0
  (track "Hey Love" "Stevie Wonder" "Down to Earth" 162000 12 date-example-0 4 date-example-1))
(define track-example-1
  (track "A Place in the Sun" "Stevie Wonder" "Down to Earth" 172000 1 date-example-0 4 date-example-2))
(define track-example-2
  (track "As" "Stevie Wonder" "Songs in the Key of Life" 428000 16 date-example-2 15 date-example-3))
(define track-example-3
  (track "Ebony Eyes" "Stevie Wonder" "Songs in the Key of Life" 251000 19 date-example-2 15 date-example-4))
(define track-example-4
  (track "DNA" "Kendrick Lamar" "DAMN." 185000 2 date-example-4 2 date-example-5))
(define track-example-5
  (track "Nobody Speak" "DJ Shadow" "The Mountain will Fall" 196000 2 date-example-6 20 date-example-7))

(define l-track-example-0
  (list track-example-0
        track-example-1
        track-example-2
        track-example-3
        track-example-4
        track-example-5))

;;;; functions

;; String Date [List-of Track] -> [List-of Track]
;; extracts Tracks from l-tracks that:
;; - matches an-album
;; - was last played after a-date

(check-expect
 (select-album-date "Down to Earth" date-example-0 '())
 '())
(check-expect
 (select-album-date "Down to Earth" date-example-0 l-track-example-0)
 (list track-example-0 track-example-1))
(check-expect
 (select-album-date "Down to Earth" date-example-1 l-track-example-0)
 (list track-example-1))

(define (select-album-date an-album a-date l-track)
  (local ((define tracks-of-an-album
            (filter
             (lambda (a-track)
               (string=? an-album (track-album a-track)))
             l-track))
          ;; [List-of Track] -> [List-of Track]
          ;; returns a list of tracks from the same album that
          ;; have been played after a-date
          (define (tracks-played-after-a-date l-track/same-album)
            (filter
             (lambda (a-track)
               (< (date->seconds a-date) (date->seconds (track-played a-track))))
             l-track/same-album)))
    (tracks-played-after-a-date tracks-of-an-album)))

;; [List-of Track] -> [List [List-of Track]]
;; returns a list of list of tracks, one per album

(check-expect (select-albums '()) '())
(check-expect
 (select-albums l-track-example-0)
 `((,track-example-0 ,track-example-1)
   (,track-example-2 ,track-example-3)
   (,track-example-4)
   (,track-example-5)))

(define (select-albums l-track)
  (local (;; Track [List-of [List-of Track]] -> [List-of [List-of Track]]
          ;; inserts a-track into the appropriate album and then
          ;; conses it onto a list of list of Tracks
          (define (insert-into-album a-track l-l-track)
            (cond [(empty? l-l-track) (list (list a-track))]
                  [else (cond [(string=? (track-album a-track) (track-album (caar l-l-track)))
                               (cons (cons a-track (car l-l-track)) (cdr l-l-track))]
                              [else (cons (list a-track) l-l-track)])])))
    (foldr insert-into-album '() l-track)))

;;;; application

(test)
