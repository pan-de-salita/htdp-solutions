#lang htdp/bsl+
(require test-engine/racket-tests)

;;; data definitions

(define-struct gp [name score])
;; a GamePlayer is a structure:
;;     (make-gp String Number)
;; i.e. (make-gp p s) represents player p who scored
;; a maximum of s points
;; examples:
(define gp0 (make-gp "tam" 399))
(define gp1 (make-gp "tim" 411))
(define gp2 (make-gp "tom" 498))

;; a List-of-GPs is a one of:
;; - '()
;; - (cons GP List-of-GPs)
;; i.e. a list of GamePlayer instances
(define alogp0 '())
(define alogp1 (list gp0))
(define alogp2 (list gp0 gp1 gp2))
(define alogp3 (list gp1 gp2 gp0))
(define alogp4 (list gp2 gp1 gp0))

;; an NE-list-of-GPs is a one of:
;; - (cons GP '())
;; - (cons GP List-of-GPs)
;; i.e. a non-empty list of GamePlayer instances
(define anelogp0 alogp1)
(define anelogp1 alogp2)
(define anelogp2 alogp3)
(define anelogp3 alogp4)

;;; functions

;; List-of-GPs -> List-of-GPs
;; sorts a List-of-GPs alogp by score

(check-expect (sort>/gp alogp0) alogp0)
(check-satisfied (sort>/gp alogp1) sorted>/gp?)
(check-satisfied (sort>/gp alogp2) sorted>/gp?)
(check-satisfied (sort>/gp alogp3) sorted>/gp?)
(check-satisfied (sort>/gp alogp4) sorted>/gp?)

(define (sort>/gp alogp)
  (cond [(empty? alogp) '()]
        [else (insert/gp (car alogp) (sort>/gp (cdr alogp)))]))

;; GP List-of-GPs -> List-of-GPs
;; inserts a GamePlayer gp into a List-of-GPs alogp
;; that's been sorted by score

(check-expect (insert/gp gp1 '()) (list gp1))
(check-expect (insert/gp gp1 (list gp0)) (list gp1 gp0))
(check-expect (insert/gp gp1 (list gp2)) (list gp2 gp1))
(check-expect (insert/gp gp1 (list gp2 gp0)) alogp4)

(define (insert/gp gp alogp)
  (cond [(empty? alogp) (cons gp '())]
        [else (if (>= (gp-score gp) (gp-score (car alogp)))
                  (cons gp alogp)
                  (cons (car alogp) (insert/gp gp (cdr alogp))))]))

;; NE-list-of-numbers -> Boolean
;; checks if the scores in an NE-list-of-GPs anelogp
;; are sorted in descending order

(check-expect (sorted>/gp? anelogp0) #t)
(check-expect (sorted>/gp? anelogp1) #f)
(check-expect (sorted>/gp? anelogp2) #f)
(check-expect (sorted>/gp? anelogp3) #t)

(define (sorted>/gp? anelogp)
  (cond [(empty? (cdr anelogp)) #t]
        [else (and (>= (gp-score (car anelogp)) (gp-score (cadr anelogp)))
                   (sorted>/gp? (cdr anelogp)))]))

;;; application

(test)
