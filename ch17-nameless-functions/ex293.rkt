#lang racket
(require test-engine/racket-tests)

;; [X] X [List-of X] -> [Maybe [List-of X]]
;; returns the first sublist of l that starts
;; with x, #false otherwise

(check-expect (find 2 '()) #f)
(check-expect (find 2 '(1 2 3)) '(2 3))
(check-expect (find 2 '(1 3 5)) #f)
(check-expect (find 2 '(1 2 3 2 2 2 2 2)) '(2 3 2 2 2 2 2))

(check-satisfied (find 2 '()) (found?.v4 2 '()))
(check-satisfied (find 2 '(1 2 3)) (found?.v4 2 '(1 2 3)))
(check-satisfied (find 2 '(1 3 5)) (found?.v4 2 '(1 3 5)))
(check-satisfied (find 2 '(1 2 3 2 2 2 2 2)) (found?.v4 2 '(1 2 3 2 2 2 2 2)))

(define (find x l)
  (cond [(empty? l) #f]
        [else (cond [(equal? (car l) x) l]
                    [else (find x (cdr l))])]))

;; X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
;; is a specification for find; returns a function that checks if:
;; - l-test contains x
;; - l-test is the sublist of l-ref that starts with x

(check-expect [(found? 2 '(1 2 3)) '()] #f)
(check-expect [(found? 2 '(1 2 3)) (find 2 '(1 2 3))] #t)
(check-expect [(found? 2 '(1 3 5)) (find 2 '(1 3 5))] #t)

(check-expect [(found? 2 '(1 2 3 2 2 2 2 2)) (find 2 '(1 2 3 2 2 2 2 2))] #t)
(check-expect [(found? 2 '(1 2 3 2 2 2 2 2)) '(2 2 2 2 2)] #f)

(check-expect [(found? 3 '(5 6 1 8 3 2 0 52)) '(3 2 0 52)] #t)
(check-expect [(found? 3 '(5 6 1 8 3 2 0 52)) '(3 2 52)] #f)
(check-expect [(found? 3 '(5 6 1 8 3 2 0 52)) '(8 3 2 0 52)] #f)
(check-expect [(found? 3 '(5 6 1 8 3 2 52)) '(3 2 0 52)] #f)
(check-expect [(found? 7 '(5 6 1 8 3 2 52)) #f] #t)

(define (found? x l-ref)
  (lambda (l-test)
    (equal? l-test (member x l-ref))))

;; X [List-of X] -> [ [Maybe [List-of X]] -> Boolean ]
;; like found?, is a specification for find; returns a function
;; that checks if test is the first sublist of l-ref that starts with x

(check-expect [(found?.v4 2 '(1 2 3)) '()] #f)
(check-expect [(found?.v4 2 '(1 2 3)) '(2 3)] #t)
(check-expect [(found?.v4 2 '(1 3 5)) (find 2 '(1 3 5))] #t)

(check-expect [(found?.v4 2 '(1 2 3 2 2 2 2 2)) (find 2 '(1 2 3 2 2 2 2 2))] #t)
(check-expect [(found?.v4 2 '(1 2 3 2 2 2 2 2)) '(2 2 2 2 2)] #f)

(check-expect [(found?.v4 3 '(5 6 1 8 3 2 0 52)) '(3 2 0 52)] #t)
(check-expect [(found?.v4 3 '(5 6 1 8 3 2 0 52)) '(3 2 52)] #f)
(check-expect [(found?.v4 3 '(5 6 1 8 3 2 0 52)) '(8 3 2 0 52)] #f)
(check-expect [(found?.v4 3 '(5 6 1 8 3 2 52)) '(3 2 0 52)] #f)
(check-expect [(found?.v4 7 '(5 6 1 8 3 2 52)) #f] #t)

(define (found?.v4 x l-ref)
  (lambda (test)
    (local (;; [Maybe Boolean] -> Boolean
            ;; checks if x does not figure in l
            (define (x-not-found? maybe-boolean)
              (if (false? maybe-boolean)
                  (false? (member x l-ref))
                  #f))
            ;; [Maybe [List-of X]] -> Boolean
            ;; checks if maybe-l is the first sublist of l-ref that starts with x
            (define (first-sublist-starting-with-x? maybe-l l-ref)
              (cond [(or (empty? l-ref) (empty? maybe-l)) #f]
                    [else (if (equal? x (car l-ref))
                              (equal? maybe-l l-ref)
                              (first-sublist-starting-with-x? maybe-l (cdr l-ref)))])))
      (or (x-not-found? test)
          (first-sublist-starting-with-x? test l-ref)))))

(test)

;;;; wrong solutions --------------------------------------------------------

#|
;; X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
;; like found?, is a specification for find; returns a function
;; that checks if:
;; - result-from-find contains x
;; - result-from-find is the first sublist of l-ref that starts with x

(check-expect [(found?.v2 2 '(1 2 3)) (find 2 '(1 2 3))] #t)
(check-expect [(found?.v2 2 '(1 3 5)) (find 2 '(1 3 5))] #t)
(check-expect [(found?.v2 2 '(1 2 3 2 2 2 2 2)) (find 2 '(1 2 3 2 2 2 2 2))] #t)
(check-expect [(found?.v2 2 '(1 2 3 2 2 2 2 2)) '(2 2 2 2 2)] #t) ;; should return #f
(check-expect [(found?.v2 3 '(5 6 1 8 3 2 0 52)) '(3 2 0 52)] #t)
(check-expect [(found?.v2 3 '(5 6 1 8 3 2 0 52)) '(3 2 52)] #f)
(check-expect [(found?.v2 3 '(5 6 1 8 3 2 0 52)) '(8 3 2 0 52)] #f)
(check-expect [(found?.v2 3 '(5 6 1 8 3 2 52)) '(3 2 0 52)] #f)
(check-expect [(found?.v2 7 '(5 6 1 8 3 2 52)) #f] #t)

(define (found?.v2 x l-ref)
  (lambda (result-from-find)
    (local (;; [List-of X] -> Boolean
            ;; checks if l-x
            ;; - contains x as its first item
            ;; - is a sublist of l-ref
            (define (valid-sublist-of-l-ref? l-from-find)
              (local ((define number-of-items-to-trim
                        (- (length l-ref) (length l-from-find))))
                (and (equal? (car l-from-find) x)
                     (equal? l-from-find
                             [(apply-n-times cdr number-of-items-to-trim) l-ref])))))
      (or ;; checks if:
          ;; - result-from-find is equal to #f
          ;; - x does NOT figure in l-ref
          (or (false? result-from-find)
              (boolean? (member x l-ref)))
          ;; checks if result-from-find is the first sublist
          ;; of l-ref that starts with x
          (valid-sublist-of-l-ref? result-from-find)))))

;; [X] [X -> Y] Number -> [[List-of X] -> [List-of Y]]
;; returns a function that applies F n times to a list l-x

(check-expect [(apply-n-times cdr 3) '(1 2 3 4 5)] '(4 5))
(check-expect [(apply-n-times cdr 2) '(1 2 3 2 2 2 2 2)] '(3 2 2 2 2 2))

(define (apply-n-times F n)
  (lambda (l-x)
    (if (zero? n)
        l-x
        (F [(apply-n-times F (sub1 n)) l-x]))))

;; X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
;; like found?, is a specification for find; returns a function
;; that checks if:
;; - result-from-find contains x
;; - result-from-find is the first sublist of l-ref that starts with x
;; logic from S8A: https://github.com/S8A/htdp-exercises/blob/master/ex294.rkt

(check-expect [(found?.v3 2 '(1 2 3)) '(2 3)] #t)
(check-expect [(found?.v3 2 '(1 3 5)) (find 2 '(1 3 5))] #t)
(check-expect [(found?.v3 2 '(1 2 3 2 2 2 2 2)) (find 2 '(1 2 3 2 2 2 2 2))] #t)
(check-expect [(found?.v3 2 '(1 2 3 2 2 2 2 2)) '(2 2 2 2 2)] #t) ;; shoud return #f
(check-expect [(found?.v3 3 '(5 6 1 8 3 2 0 52)) '(3 2 0 52)] #t)
(check-expect [(found?.v3 3 '(5 6 1 8 3 2 0 52)) '(3 2 52)] #f)
(check-expect [(found?.v3 3 '(5 6 1 8 3 2 0 52)) '(8 3 2 0 52)] #f)
(check-expect [(found?.v3 3 '(5 6 1 8 3 2 52)) '(3 2 0 52)] #f)
(check-expect [(found?.v3 7 '(5 6 1 8 3 2 52)) #f] #t)

(define (found?.v3 x l-ref)
  (lambda (result-from-find)
    (local (;; [NEList-of X] [List-of X] -> Boolean
            ;; checks if l-x is a sublist of l-y
            (define (valid-sublist? l-x l-y)
              (and (<= (length l-x) (length l-y))
                   (cond [(empty? (cdr l-x)) (equal? (car l-x) (car l-y))]
                         [else (cond [(equal? (car l-x) (car l-y))
                                      (valid-sublist? (cdr l-x) (cdr l-y))]
                                     [else (valid-sublist? l-x (cdr l-y))])]))))
      (cond [(false? result-from-find)
             (boolean? (member x l-ref))]
            [else (and (not (empty? result-from-find))
                       (equal? x (car result-from-find))
                       (valid-sublist? result-from-find l-ref))]))))
|#
