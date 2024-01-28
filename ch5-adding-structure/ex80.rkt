#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define-struct movie [title director year])
;; a Movie is a structure:
;;  (make-movie String String String)

(define movie-example-1 (make-movie "The Wailing" "Na Hong-jin" "2016"))
(define movie-example-2 (make-movie "Noroi" "Koji Shiraishi" "2005"))

;; a UserCommand is one of:
;; - "t" | indicating request for movie title
;; - "d" | indicating request for movie director
;; - "y" | indicating request for movie year of release

;; Movie -> String
;; returns either the title, director, or year of
;; release of a movie depending on UserCommand
(check-expect (movie-trivia movie-example-1 "t") (movie-title movie-example-1))
(check-expect (movie-trivia movie-example-2 "d") (movie-director movie-example-2))
(check-expect (movie-trivia movie-example-1 "y") (movie-year movie-example-1))
(check-expect (movie-trivia movie-example-2 " ") "incorrect prompt")

(define (movie-trivia movie-entry trivia-requested)
  (cond [(key=? trivia-requested "t") (movie-title movie-entry)]
        [(key=? trivia-requested "d") (movie-director movie-entry)]
        [(key=? trivia-requested "y") (movie-year movie-entry)]
        [else "incorrect prompt"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct pet [name number])
;; a Pet is a structure:
;;  (make-pet String PositiveNumber)

(define pet-example-1 (make-pet "Felix" 0))
(define pet-example-2 (make-pet "Gilles" 1))
(define pet-example-3 (make-pet "Hegel" 2))

;; Pet -> String
;; returns a pet's name according to its number
(check-expect (pet-name pet-example-1) "Felix")
(check-expect (pet-name pet-example-2) "Gilles")
(check-expect (pet-name pet-example-3) "Hegel")

(define (name-of-pet pet-entry)
  (pet-name pet-entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct CD [artist title price])
;; a CD is a structure:
;;  (make-CD String String PositiveNumber)

(define CD-example-1 (make-CD "Section 80" "Kendrick Lamar" 9.49))
(define CD-example-2 (make-CD "Drunk" "Thundercat" 14.99))
(define CD-example-3 (make-CD "Sometimes I Might Be Introvert" "Little Simz" 16.86))

;; CD -> Number
;; determines if a CD is one of:
;; - "affordable" | its price is below $10
;; - "manageable" | its price is above $10 but below $15
;; - "pricey" | its price exceeds $15
(check-expect (CD-price-range CD-example-1) "affordable")
(check-expect (CD-price-range CD-example-2) "manageable")
(check-expect (CD-price-range CD-example-3) "pricey")

(define (CD-price-range CD-entry)
  (cond [(< (CD-price CD-entry) 10.00) "affordable"]
        [(and (>= (CD-price CD-entry) 10.00) (< (CD-price CD-entry) 15.00)) "manageable"]
        [(>= (CD-price CD-entry) 15.00) "pricey"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a Size is one of the following Strings:
;; - "XS"
;; - "S"
;; - "M"
;; - "L"
;; - "XL"
;; interpretation: sweater size

(define-struct sweater [material size color])
;; a Sweater is a structure:
;;  (make-sweater String String String)

(define sweater-example-1 (make-sweater "cotton" "XL" "black"))
(define sweater-example-2 (make-sweater "wool" "M" "red"))
(define sweater-example-3 (make-sweater "leather" "XS" "green"))

;; Sweater -> String
;; returns what size in which a sweater is available
(check-expect (sweater-size-available sweater-example-1) "XL")
(check-expect (sweater-size-available sweater-example-2) "M")
(check-expect (sweater-size-available sweater-example-3) "XS")

(define (sweater-size-available sweater-entry)
  (sweater-size sweater-entry))

(test)
