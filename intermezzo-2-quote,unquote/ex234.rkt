#lang racket
(require test-engine/racket-tests)
(require 2htdp/web-io)

;;;; data definitions and constants ----------------------------------

(define HTML-TABLE-TITLE "your songs ranked")

;; a Song is a String.
;; describes a song artist and one of their songs.
(define song-0 "Asia: Heat of the Moment")
(define song-1 "U2: One")
(define song-2 "The White Stripes: Seven Nation Army")
(define list-of-songs `(,song-0 ,song-1 ,song-2))

;; a Song-Rank is one of:
;; - '()
;; - '(String Song)
;; describes a Song's rank and the Song itself.
(define song-rank-0 `("1" ,song-0))
(define song-rank-1 `("2" ,song-1))
(define song-rank-2 `("3" ,song-2))
(define list-of-song-ranks `(,song-rank-0 ,song-rank-1 ,song-rank-2))

;;;; functions -------------------------------------------------------

;; List-of-Songs -> HTML-Table
;; returns an HTML-Table describing a given List-of-Songs as a List-of-Song-Ranks.

(check-expect
 (songs->html-table list-of-songs)
 `(html
   (head
    (title ,HTML-TABLE-TITLE)
    (meta ((http-equiv "content-type")
           (content "text -html"))))
   (body
    (h1 ,HTML-TABLE-TITLE)
    (table ((border "1"))
           (tr (td "1") (td "Asia: Heat of the Moment"))
           (tr (td "2") (td "U2: One"))
           (tr (td "3") (td "The White Stripes: Seven Nation Army"))))))

(define (songs->html-table a-list-of-songs)
  `(html
    (head
     (title ,HTML-TABLE-TITLE)
     (meta ((http-equiv "content-type")
            (content "text -html"))))
    (body
     (h1 ,HTML-TABLE-TITLE)
     (table ((border "1"))
            ,@(song-ranks->rows (ranking a-list-of-songs))))))

;; List-of-Songs -> List-of-Song-Ranks
;; returns a List-of-Songs with rankings consed onto each Song within.
;; ranking is based on each Song's order in a given List-of-Songs.

(check-expect (ranking list-of-songs) list-of-song-ranks)

(define (ranking a-list-of-songs)
  (reverse (add-ranks (reverse a-list-of-songs))))

;; List-of-Songs -> List-of-Songs-Ranks
;; helper function for ranking. conses a ranking to each Song in a
;; List-of-Songs.

(check-expect (add-ranks (reverse list-of-songs)) (reverse list-of-song-ranks))

(define (add-ranks a-list-of-songs)
  (cond [(empty? a-list-of-songs) '()]
        [else (cons (list (number->string (length a-list-of-songs)) (car a-list-of-songs))
                    (add-ranks (cdr a-list-of-songs)))]))

;; List-of-Song-Ranks -> ... nested list ...
;; returns a given List-of-Song-Ranks as a HTML table rows.

(check-expect
 (song-ranks->rows list-of-song-ranks)
 `((tr (td "1") (td "Asia: Heat of the Moment"))
   (tr (td "2") (td "U2: One"))
   (tr (td "3") (td "The White Stripes: Seven Nation Army"))))

(define (song-ranks->rows a-list-of-song-ranks)
  (cond [(empty? a-list-of-song-ranks) '()]
        [else (cons (cells->row (song-rank->cells (car a-list-of-song-ranks)))
                    (song-ranks->rows (cdr a-list-of-song-ranks)))]))

;; ... nested list ... -> ... nested list ...
;; returns a list of HTML table cells as a table row.

(check-expect
 (cells->row `((td "1") (td "Asia: Heat of the Moment")))
 `(tr (td "1") (td "Asia: Heat of the Moment")))

(define (cells->row cells)
  `(tr ,@cells))

;; Song-Rank -> ... nested list ...
;; returns a given Song-Rank as a list of HTML table cells.

(check-expect (song-rank->cells song-rank-0) `((td "1") (td "Asia: Heat of the Moment")))

(define (song-rank->cells a-song-rank)
  (cond [(empty? a-song-rank) '()]
        [else (cons (string->cell (car a-song-rank))
                    (song-rank->cells (cdr a-song-rank)))]))

;; String -> ... nested list ...
;; returns a String as an HTML table cell.

(check-expect (string->cell "1") `(td "1"))
(check-expect (string->cell "Asia: Heat of the Moment") `(td "Asia: Heat of the Moment"))

(define (string->cell a-string)
  `(td ,a-string))

;;;; application -----------------------------------------------------

(test)
