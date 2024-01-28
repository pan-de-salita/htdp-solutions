#lang racket
(require test-engine/racket-tests)

;; [X] [X -> Boolean] Number -> Boolean
;; functions just like andmap

(check-expect (and-map.v1 (lambda (x) (> (- 9 x) 0)) 10)
              (for/and ([i 10]) (> (- 0 i) 0)))

(define (and-map.v1 cmp i)
  (for/and ([iterator i]) (cmp iterator)))

;; [X] [X -> Boolean] Number -> Boolean
;; functions just like andmap

(check-expect (and-map.v2 (lambda (x) (> (- 9 x) 0)) 10)
              (for/and ([i 10]) (> (- 0 i) 0)))

(define (and-map.v2 cmp i)
  (andmap cmp (build-list i (lambda (iterator) iterator))))

;; [X] [X -> Boolean] Number -> Boolean
;; functions just like andmap

(check-expect (and-map.v3 (lambda (x) (> (- 9 x) 0)) 10)
              (for/and ([i 10]) (> (- 0 i) 0)))

(define (and-map.v3 cmp i)
  (local ((define iterator (build-list i (lambda (x) x)))
          ;; [List-of X] -> Boolean
          (define (for-and l)
            (if (empty? l)
                #t
                (and (cmp (car l))
                     (for-and (cdr l))))))
    (for-and iterator)))

;; [X] [X -> Boolean] Number -> Boolean
;; functions just like ormap

(check-expect (or-map (lambda (x) (if (= (- 9 x) 0) x #f)) 10)
              (for/or ([i 10]) (if (= (- 9 i) 0) i #f)))

(define (or-map cmp i)
  (for/or ([iterator i]) (cmp iterator)))

;; [X] [X -> Boolean] Number -> Boolean
;; functions just like ormap

(check-expect (or-map.v2 (lambda (x) (if (= (- 9 x) 0) x #f)) 10)
              (for/or ([i 10]) (if (= (- 9 i) 0) i #f)))

(define (or-map.v2 cmp i)
  (ormap cmp (build-list i (lambda (iterator) iterator))))

;; [X] [X -> Boolean] Number -> Boolean
;; functions just like ormap

(check-expect (or-map.v3 (lambda (x) (if (= (- 9 x) 0) x #f)) 10)
              (for/or ([i 10]) (if (= (- 9 i) 0) i #f)))

(define (or-map.v3 cmp i)
  (local ((define iterator (build-list i (lambda (x) x)))
          ;; [List-of Number] -> Boolean
          (define (for-or l)
            (if (empty? l)
                #t
                (or (cmp (car l))
                    (for-or (cdr l))))))
    (for-or iterator)))

(test)
