#lang racket
(require test-engine/racket-tests
         lang/posn)

;; distances in terms of pixels
(define WIDTH 300)
(define HEIGHT 300)
(define FALSE-POSN (make-posn (/ WIDTH 2) (/ HEIGHT 2)))

;; N -> [List-of Posn]
;; generates n random Posns in [0,WIDTH) BY [0,HEIGHT)

(check-satisfied (random-posns 0)
                 (n-inside-playground? 0))
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))

(define (random-posns n)
  (build-list
   n
   (lambda (i)
     (make-posn (random WIDTH) (random HEIGHT)))))

;; N -> [List-of Posn]
;; generates n  Posns in [0,WIDTH) BY [0,HEIGHT)

(check-satisfied (random-posns/bad 0)
                 (n-inside-playground? 0))
(check-satisfied (random-posns/bad 3)
                 (n-inside-playground? 3))

(define (random-posns/bad n)
  (build-list n (lambda (i) FALSE-POSN)))

;; N -> [Maybe [List-of Posn] -> Boolean]
;; specification for random-posns; returns a function
;; that checks whether i:
;; - is a list of length n
;; - contains Posns within (* WIDTH HEIGHT)

(check-expect [(n-inside-playground? 0) '()] #t)
(check-expect
 [(n-inside-playground? 3)
  `(,(make-posn 50 100)
    ,(make-posn 100 200)
    ,(make-posn 200 150))]
 #t)
(check-expect
 [(n-inside-playground? 3)
  `(,(make-posn 50 100)
    ,(make-posn 100 200)
    ,(make-posn 200 400))]
 #f)
(check-expect
 [(n-inside-playground? 3)
  `(,(make-posn 50 100)
    ,(make-posn 100 200))]
 #f)

(define (n-inside-playground? n)
  (lambda (i)
    (local (;; [Maybe [List-of Posn]] -> Boolean
            ;; checks whether maybe-l-posn is a list
            ;; of length n
            (define (list-of-length-n? maybe-l-posn)
              (and (list? maybe-l-posn) (= (length maybe-l-posn) n)))
            ;; [Maybe [List-of Posn]] -> Boolean
            ;; checks whether all posns on maybe-l-posn
            ;; are within (* WIDTH HEIGHT)
            (define (posns-inside-playground? maybe-l-posn)
              (andmap
               (lambda (maybe-posn)
                 (and (posn? maybe-posn)
                      (< (posn-x maybe-posn) WIDTH)
                      (< (posn-y maybe-posn) HEIGHT)))
               maybe-l-posn)))
      (and (list-of-length-n? i) (posns-inside-playground? i)))))

(test)
