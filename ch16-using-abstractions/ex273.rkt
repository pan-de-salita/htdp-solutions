#lang racket
(require test-engine/racket-tests)

;; [X Y] [X -> Y] [List-of X] -> [List-of Y]
;; own-map works just like map

(check-expect (own-map + '()) '())
(check-expect (own-map number->string '(1 2 3 4 5)) '("1" "2" "3" "4" "5"))
(check-expect (own-map string->number '("1" "2" "3" "4" "5")) '(1 2 3 4 5))

(define (own-map a-function a-list)
  (local (;; X Y -> Y
          ;; applies a-function to X together with
          ;; Y to arrive at Y
          (define (map-for-fold list-item initial-item)
            (cons (a-function list-item) initial-item)))
    (foldr map-for-fold '() a-list)))

;; [X Y] [X -> Y] [List-of X] -> [List-of Y]
;; own-map.v2, like own-map, works just like map

(check-expect (own-map.v2 + '()) '())
(check-expect (own-map.v2 number->string '(1 2 3 4 5)) '("1" "2" "3" "4" "5"))
(check-expect (own-map.v2 string->number '("1" "2" "3" "4" "5")) '(1 2 3 4 5))

(define (own-map.v2 a-function a-list)
  (foldr (lambda (list-item initial-item) (cons (a-function list-item) initial-item)) '() a-list))

(test)
