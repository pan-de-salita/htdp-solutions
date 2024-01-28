#lang racket
(require test-engine/racket-tests)

;; [X] X [List-of X] -> [Maybe [List-of X]]
;; returns the first sublist of l that starts
;; with x, #f otherwise

(define (find x l)
  (cond [(empty? l) #f]
        [else (if (equal? (car l) x)
                  l
                  (find x (cdr l)))]))

(check-expect (find "a" '()) #f)
(check-expect (find "a" '("a" "b" "c" "a" "a" "a")) '("a" "b" "c" "a" "a" "a"))
(check-expect (find "b" '("a" "b" "c" "a" "a" "a")) '("b" "c" "a" "a" "a"))
(check-expect (find "c" '("a" "b" "c" "a" "a" "a")) '("c" "a" "a" "a"))
(check-expect (find "d" '("a" "b" "c" "a" "a" "a")) #f)
(check-satisfied (find "a" '()) (found? "a" '()))
(check-satisfied (find "a" '("a" "b" "c" "a" "a" "a")) (found? "a" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (find "b" '("a" "b" "c" "a" "a" "a")) (found? "b" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (find "c" '("a" "b" "c" "a" "a" "a")) (found? "c" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (find "d" '("a" "b" "c" "a" "a" "a")) (found? "d" '("a" "b" "c" "a" "a" "a")))

;; [X] X [List-of X] -> [[Maybe [List-of X]] -> Boolean]
;; specification for find, returns a function that checks if:
;; - y is #f (x does not figure in l)
;; - y is the first sublist of l that starts with x

(define (found? x l)
  (lambda (y)
    (local (;; [List-of X] -> Boolean
            ;; checks if y-list is the first sublist of l that starts with x
            (define (first-sublist-starting-with-x? y-list ref-list)
              (cond [(> (length y-list) (length ref-list)) #f]
                    [else (if (equal? (car ref-list) x)
                              (equal? y-list ref-list)
                              (first-sublist-starting-with-x? y-list (cdr ref-list)))])))
      (cond [(boolean? y) (and (false? y) (false? (member x l)))]
            [(cons? y) (first-sublist-starting-with-x? y l)]
            [else #f]))))

(check-expect [(found? "a" '()) (find "a" '())] #t)
(check-expect [(found? "a" '("a" "b" "c" "a" "a" "a")) (find "a" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(found? "b" '("a" "b" "c" "a" "a" "a")) (find "b" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(found? "c" '("a" "b" "c" "a" "a" "a")) (find "c" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(found? "d" '("a" "b" "c" "a" "a" "a")) (find "d" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(found? "a" '("a" "b" "c" "a" "a" "a")) '("a" "a" "a")] #f)
(check-expect [(found? "a" '("a" "b" "c" "a" "a" "a")) '("a" "b" "c")] #f)
(check-expect [(found? "a" '("a" "b" "c" "a" "a" "a")) '()] #f)

;; [X] X [List-of X] -> [Maybe Number]
;; determines the index of the first occurrence of x in l, #f otherwise

(define (index x l)
  (cond [(empty? l) #f]
        [else (if (equal? (car l) x)
                  0
                  (local ((define i (index x (cdr l))))
                    (if (boolean? i) i (add1 i))))]))

(check-expect (index "a" '()) #f)
(check-expect (index "a" '("a" "b" "c" "a" "a" "a")) 0)
(check-expect (index "b" '("a" "b" "c" "a" "a" "a")) 1)
(check-expect (index "c" '("a" "b" "c" "a" "a" "a")) 2)
(check-expect (index "d" '("a" "b" "c" "a" "a" "a")) #f)
(check-satisfied (index "a" '()) (is-index? "a" '()))
(check-satisfied (index "a" '("a" "b" "c" "a" "a" "a")) (is-index? "a" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "b" '("a" "b" "c" "a" "a" "a")) (is-index? "b" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "c" '("a" "b" "c" "a" "a" "a")) (is-index? "c" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "d" '("a" "b" "c" "a" "a" "a")) (is-index? "d" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "a" '()) (is-index?.v2 "a" '()))
(check-satisfied (index "a" '("a" "b" "c" "a" "a" "a")) (is-index?.v2 "a" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "b" '("a" "b" "c" "a" "a" "a")) (is-index?.v2 "b" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "c" '("a" "b" "c" "a" "a" "a")) (is-index?.v2 "c" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "d" '("a" "b" "c" "a" "a" "a")) (is-index?.v2 "d" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "a" '()) (is-index?.v3 "a" '()))
(check-satisfied (index "a" '("a" "b" "c" "a" "a" "a")) (is-index?.v3 "a" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "b" '("a" "b" "c" "a" "a" "a")) (is-index?.v3 "b" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "c" '("a" "b" "c" "a" "a" "a")) (is-index?.v3 "c" '("a" "b" "c" "a" "a" "a")))
(check-satisfied (index "d" '("a" "b" "c" "a" "a" "a")) (is-index?.v3 "d" '("a" "b" "c" "a" "a" "a")))

;; [X] X [List-of X] -> [[Maybe Number] -> Boolean]
;; specification for index, returns a function that checks if:
;; - y is #f (x does not figure in l)
;; - y is the index of the first occurrence of x in l

(define (is-index? x l)
  (lambda (y)
    (local (;; Number -> Boolean
            ;; checks if y-index is the index of the first occurrence of x in l
            (define (index-of-first-occurrence-of-x? y-index)
              (and (< y-index (length l))
                   (false? (member x
                                   ;; all items before (list-ref l y-index):
                                   (build-list y-index (lambda (index) (list-ref l index))))))))
      (cond [(boolean? y) (and (false? y) (false? (member x l)))]
            [(number? y) (index-of-first-occurrence-of-x? y)]
            [else #f]))))

(check-expect [(is-index? "a" '()) (index "a" '())] #t)
(check-expect [(is-index? "a" '("a" "b" "c" "a" "a" "a")) (index "a" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index? "b" '("a" "b" "c" "a" "a" "a")) (index "b" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index? "c" '("a" "b" "c" "a" "a" "a")) (index "c" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index? "d" '("a" "b" "c" "a" "a" "a")) (index "d" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index? "a" '("a" "b" "c" "a" "a" "a")) 3] #f)
(check-expect [(is-index? "a" '("a" "b" "c" "a" "a" "a")) 10] #f)
(check-expect [(is-index? "a" '("a" "b" "c" "a" "a" "a")) '()] #f)

;; [X] X [List-of X] -> [[Maybe Number] -> Boolean]
;; like is-index, is specification for index, returns a function that checks if:
;; - y is #f (x does not figure in l)
;; - y is the index of the first occurrence of x in l

(define (is-index?.v2 x l)
  (lambda (y)
    (local (;; Number -> Boolean
            ;; checks if y-index is the index of the first occurrence of x in l
            (define (index-of-first-occurrence-of-x? y-index)
              (and (< y-index (length l))
                   (equal? (member x l) [(apply-n-times cdr y-index) l]))))
      (cond [(boolean? y) (and (false? y) (false? (member x l)))]
            [(number? y) (index-of-first-occurrence-of-x? y)]
            [else #f]))))

(check-expect [(is-index?.v2 "a" '()) (index "a" '())] #t)
(check-expect [(is-index?.v2 "a" '("a" "b" "c" "a" "a" "a")) (index "a" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index?.v2 "b" '("a" "b" "c" "a" "a" "a")) (index "b" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index?.v2 "c" '("a" "b" "c" "a" "a" "a")) (index "c" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index?.v2 "d" '("a" "b" "c" "a" "a" "a")) (index "d" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index?.v2 "a" '("a" "b" "c" "a" "a" "a")) 3] #f)
(check-expect [(is-index?.v2 "a" '("a" "b" "c" "a" "a" "a")) 10] #f)
(check-expect [(is-index?.v2 "a" '("a" "b" "c" "a" "a" "a")) '()] #f)

;; [X -> Y] Number -> [[List-of X] [List-of Y]]
;; returns a function that applies F n times to l

(check-expect [(apply-n-times cdr 0) '(1 2 3)] '(1 2 3))
(check-expect [(apply-n-times cdr 1) '(1 2 3)] '(2 3))
(check-expect (car [(apply-n-times cdr 1) '(1 2 3)]) 2)

(define (apply-n-times F n)
  (lambda (l)
    (if (zero? n)
        l
        (F [(apply-n-times F (sub1 n)) l]))))

;; [X] X [List-of X] -> [[Maybe Number] -> Boolean]
;; like is-index, is specification for index, returns a function that checks if:
;; - y is #f (x does not figure in l)
;; - y is the index of the first occurrence of x in l

(define (is-index?.v3 x l)
  (lambda (y)
    (local (;; Number -> Boolean
            ;; checks if y-index is the index of the first occurrence of x in l
            (define (index-of-first-occurrence-of-x? y-index)
              (and (< y-index (length l))
                   (if (not (false? (member x l)))
                       (= y-index
                          ;; another computation for the index of the first occurrence
                          ;; of x in l:
                          (- (length l) (length (member x l))))
                       #f))))
      (cond [(boolean? y) (and (false? y) (false? (member x l)))]
            [(number? y) (index-of-first-occurrence-of-x? y)]
            [else #f]))))

(check-expect [(is-index?.v3 "a" '()) (index "a" '())] #t)
(check-expect [(is-index?.v3 "a" '("a" "b" "c" "a" "a" "a")) (index "a" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index?.v3 "b" '("a" "b" "c" "a" "a" "a")) (index "b" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index?.v3 "c" '("a" "b" "c" "a" "a" "a")) (index "c" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index?.v3 "d" '("a" "b" "c" "a" "a" "a")) (index "d" '("a" "b" "c" "a" "a" "a"))] #t)
(check-expect [(is-index?.v3 "a" '("a" "b" "c" "a" "a" "a")) 3] #f)
(check-expect [(is-index?.v3 "a" '("a" "b" "c" "a" "a" "a")) 10] #f)
(check-expect [(is-index?.v3 "a" '("a" "b" "c" "a" "a" "a")) '()] #f)

(test)
