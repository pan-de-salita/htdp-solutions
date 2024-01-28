#lang racket
(require test-engine/racket-tests)

;; [X] [List-of X] [X X -> Boolean] -> [List-of Boolean]
;; sorts l-x according to cmp

(check-expect (sort-cmp '("c" "b") string<?) '("b" "c"))
(check-expect (sort-cmp '(2 1 3 4 6 5) <) '(1 2 3 4 5 6))
(check-satisfied (sort-cmp '("c" "b") string<?)
                 (sorted string<?))
(check-satisfied (sort-cmp '(2 1 3 4 6 5) <)
                 (sorted <))

(define (sort-cmp l-x cmp)
  (local (;; [List-of X] -> [List-of X]
          ;; produes a variant of l-to-sort sorted by cmp
          (define (insertion-sort l-to-sort)
            (cond [(empty? l-to-sort) '()]
                  [else (insert (car l-to-sort) (insertion-sort (cdr l-to-sort)))]))

          ;; X [List-of X] -> [List-of X]
          ;; inserts x into a sorted [List-of X]
          (define (insert x l-x-sorted)
            (cond [(empty? l-x-sorted) (list x)]
                  [else (cond [(cmp x (car l-x-sorted)) (cons x l-x-sorted)]
                              [else (cons (car l-x-sorted) (insert x (cdr l-x-sorted)))])])))
    (insertion-sort l-x)))

;; [X] [X X -> Boolean] -> [[List-of X] -> Boolean]
;; produces a function that determines whether
;; some list is sorted according to cmp

(check-expect [(sorted string<?) '("b" "c")] #t)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #t)

(define (sorted cmp)
  (lambda (applied-list)
    (local (;; [List-of X] -> Boolean
            ;; checks whether l-x is sorted according to cmp
            (define (sorted? l-x)
              (cond [(empty? (cdr l-x)) #t]
                    [else (and (cmp (car l-x) (cadr l-x))
                               (sorted? (cdr l-x)))])))
      (if (empty? applied-list) #t (sorted? applied-list)))))

;; [X] [List-of X] [X X -> Boolean] -> [[List-of X] -> Boolean]
;; checks whether:
;; - l-test is sorted according to cmp
;; - all items in l-test members of l-reference

(check-expect [(sorted-variant-of '("c" "b") string<?) '("b" "c")] #t)
(check-expect [(sorted-variant-of '("c" "b") string<?) '("c" "b")] #f)
(check-expect [(sorted-variant-of '("c" "b") string<?) '("a" "b")] #f)
(check-expect [(sorted-variant-of '(2 1 3 4 6 5) <) '(1 2 3 4 5 6)] #t)
(check-expect [(sorted-variant-of '(2 1 3 4 6 5) <) '(2 1 3 4 6 5)] #f)
(check-expect [(sorted-variant-of '(2 1 3 4 6 5) <) (map add1 '(1 2 3 4 5 6))] #f)

(define a-list (build-list 5000 (lambda (x) (random 1000))))
(check-random [(sorted-variant-of a-list <) (cons (- (car a-list) 1) (reverse (cdr (reverse a-list))))] #f)
(check-random [(sorted-variant-of a-list <) (sort-cmp a-list <)] #t)
(check-random [(sorted-variant-of a-list <) (sort-cmp (append a-list a-list) <)] #t)
(check-satisfied (sort-cmp a-list <) (sorted-variant-of a-list <))

(define (sorted-variant-of l-reference cmp)
  (lambda (l-test)
    (local (;; [List-of X] -> [List-of X]
            ;; removes duplicates from a-list
            (define (own-remove-duplicates a-list)
              (foldr (lambda (element-from-original-list list-in-progress)
                       (cond [(ormap (lambda (element-from-list-in-progress)
                                       (equal? element-from-list-in-progress element-from-original-list))
                                     list-in-progress)
                              list-in-progress]
                             [else (cons element-from-original-list list-in-progress)]))
                     '() ;; default list-in-progress
                     a-list ;; default original list
                     ))
            (define l-test-as-set (own-remove-duplicates l-test))
            (define l-reference-as-set (own-remove-duplicates l-reference))
            ;; [List-of X] -> Boolean
            ;; checks whether l-x is sorted according to cmp
            (define (sorted? l-x)
              (cond [(empty? (cdr l-x)) #t]
                    [else (and (cmp (car l-x) (cadr l-x))
                               (sorted? (cdr l-x)))]))
            ;; [List-of X] -> Boolean
            ;; checks whether all items in l-x are members of l-y
            (define (contains/list? l-x l-y)
              (andmap (lambda (x)
                        (not (boolean? (member x l-y))))
                      l-x)))
      (and (sorted? l-test-as-set)
           (contains/list? l-test-as-set l-reference-as-set)
           (contains/list? l-reference-as-set l-test-as-set)))))

;;;; application

(test)

;;;; deprecated

#|

alternate sort-cmp:

;; [X] [List-of X] [X X -> Boolean] -> [List-of Boolean]
;; like sort-cmp, sorts l-x according to cmp

(check-expect (sort-cmp.v2 '("c" "b") string<?) '("b" "c"))
(check-expect (sort-cmp.v2 '(2 1 3 4 6 5) <) '(1 2 3 4 5 6))

(define (sort-cmp.v2 l-x cmp)
  (cond [(empty? l-x) '()]
        [else (cond [(or (empty? (sort-cmp.v2 (cdr l-x) cmp))
                         (cmp (car l-x) (car (sort-cmp.v2 (cdr l-x) cmp))))
                     (cons (car l-x) (sort-cmp.v2 (cdr l-x) cmp))]
                    [else (sort-cmp.v2 (append (cdr l-x) (list (car l-x))) cmp)])]))

|#
