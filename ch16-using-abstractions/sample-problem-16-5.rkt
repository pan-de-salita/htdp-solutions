#lang racket
(require test-engine/racket-tests
         lang/posn)

;; [List-of Posn] -> [List-of Posn]
;; adds 3 to all x-coordinates on a list of Posns

(check-expect (add-3-to-all-x '()) '())
(check-expect
 (add-3-to-all-x (list (make-posn 0 0)))
 (list (make-posn 3 0)))
(check-expect
 (add-3-to-all-x (list (make-posn 0 0) (make-posn 0 0)))
 (list (make-posn 3 0) (make-posn 3 0)))
(check-expect
 (add-3-to-all-x (list (make-posn 3 1) (make-posn 0 0)))
 (list (make-posn 6 1) (make-posn 3 0)))

(define (add-3-to-all-x l-posn)
  (local (;; Posn -> Posn
          ;; adds 3 to the x-coordinate of a Posn
          (define (add-3-to-x a-posn)
            (make-posn (+ 3 (posn-x a-posn)) (posn-y a-posn))))
    (map add-3-to-x l-posn)))

;; [List-of Posn] -> [List-of Posn]
;; removes all Posns with y-coordinates larger than 100

(check-expect (keep-good '()) '())
(check-expect
 (keep-good (list (make-posn 0 110) (make-posn 0 60)))
 (list (make-posn 0 60)))

(define (keep-good l-posn)
  (local (;; Posn -> Boolean
          ;; checks if a Posn's y-coordinate is smaller than 100
          (define (good? a-posn) (<= (posn-y a-posn) 100)))
    (filter good? l-posn)))

;; [List-of Posn] Posn -> Boolean
;; checks whether any of a list of Posns is close to a-posn

(check-expect (close? '() (make-posn 5 6)) #f)
(check-expect (close? (list (make-posn 3 5)) (make-posn 5 6)) #t)
(check-expect (close? (list (make-posn 3 5)) (make-posn 5 10)) #f)
(check-expect (close? (list (make-posn 47 54) (make-posn 0 60)) (make-posn 50 50)) #t)

(define (close? l-posn a-posn)
  (local (;; Posn -> Boolean
          ;; checks whether p is close to pt
          (define (is-one-close? p) (close-to? p a-posn))
          ;; Posn Posn Number -> Boolean
          ;; checks whether the distance between p and q is less than d
          (define (close-to? p q [d 5])
            (local (;; Posn Posn -> Number
                    ;; returns the distance between p and q
                    (define (distance-between p q)
                      (sqrt (+ (sqr (- (posn-x p) (posn-x q)))
                               (sqr (- (posn-y p) (posn-y q)))))))
              (<= (distance-between p q) d))))
    (ormap is-one-close? l-posn)))

(test)
