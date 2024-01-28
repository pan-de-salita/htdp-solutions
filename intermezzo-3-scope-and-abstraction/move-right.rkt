#lang racket
(require test-engine/racket-tests
         lang/posn)

(define input (list (make-posn 1 1) (make-posn 10 14)))
(define expect (list (make-posn 4 1) (make-posn 13 14)))

;; [List-of Posn] Number -> [List-of Posn]
;; moves all associated objects to the right by delta-x pixels

(check-expect (move-right input 3) expect)

(define (move-right lop delta-x)
  (for/list ([p lop])
    (match p
      [(posn x y) (make-posn (+ x delta-x) y)])))

;; [List-of Posn] Number -> [List-of Posn]
;; moves all associated objects to the right by delta-x pixels

(check-expect (move-right-with-cond input 3) (move-right input 3))

(define (move-right-with-cond lop delta-x)
  (cond [(empty? lop) '()]
        [else (cons (make-posn (+ (posn-x (car lop)) delta-x) (posn-y (car lop)))
                    (move-right-with-cond (cdr lop) delta-x))]))

(test)
