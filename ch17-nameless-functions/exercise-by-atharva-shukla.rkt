#lang racket
(require test-engine/racket-tests)

;; [X] [List-of X] [X X -> Boolean] -> [List-of Boolean]
;; sorts l-x according to cmp

(check-expect (sort-cmp '("c" "b") string<?) '("b" "c"))
(check-expect (sort-cmp '(2 1 3 4 6 5) <) '(1 2 3 4 5 6))

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
            ;; removes duplicates from l
            (define (own-remove-duplicates l)
              (foldr (lambda (from-l l-occurring)
                       (cond [(ormap (lambda (from-l-occurring)
                                       (equal? from-l-occurring from-l))
                                     l-occurring)
                              l-occurring]
                             [else (cons from-l l-occurring)]))
                     '() ;; default l-in-progress
                     l ;; passes instances of from-l
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

;;;; observations on mimicking remove-duplicates:

;; [List-of X] -> [List-of X]
;; removes duplicates from l
;; NOTE: does NOT retain order of elements on l

;; returns #f:
;; (check-expect (own-remove-duplicates a-list) (remove-duplicates a-list))

(define (own-remove-duplicates l)
  (foldr (lambda (from-l l-occurring)
           (cond [(ormap (lambda (from-l-occurring)
                           (equal? from-l-occurring from-l))
                         l-occurring)
                  l-occurring]
                 [else (cons from-l l-occurring)]))
         '() ;; default l-occurring
         l)) ;; passes instances of from-l

;; [List-of X] -> [List-of X]
;; like own-remove-duplicates, removes duplicates from l
;; NOTE: DOES retain order of elements on l

(check-expect (remove-duplicates-clone a-list) (remove-duplicates a-list)) ;; returns #t

(define (remove-duplicates-clone l)
  (foldr (lambda (from-l l-occurring)
           (cons from-l
                 (filter (lambda (from-l-occurring)
                           (not (equal? from-l-occurring from-l)))
                         l-occurring)))
         '() ;; default l-occurring
         l)) ;; passes instances of from-l

;; NOTE: it was only after stumbling upon remove-duplicates-clone on
;; leontastic's github* and doing some testing that i realized that
;; own-remove-duplicates does NOT retain the order of the original list,
;; albeit performing faster.
;;
;; the reason remove-duplicates-clone is able to retain the order of
;; elements on the original list is it retains the latest (read "leftmost"
;; since foldr was used) instance of a duplicate on l WHILE removing all
;; other duplicates of the same element on the list being built. on the other
;; hand, when the latest instance of a duplicate is found on own-remove-duplicates,
;; it is ignored and the old instance is retained within the list being built.
;;
;; in other words, given three instances of a duplicate (let's call them
;; dup-latest, dup-previous, and dup-oldest) in the list '(2 1 2 3 2), with
;; the leftmost 2 being dup-latest, the middle 2 being dup-previous, and
;; the rightmost 2 being dup-oldest:
;;
;;   - remove-duplicates-clone replaces dup-previous with dup-latest, just as
;;     it replaced dup-oldest with dup-previous. thus our final output would be:
;;     '(2 1 3)
;;
;;   - own-remove-duplicates disregards dup-latest when it is detected, instead
;;     keeping its oldest instance dup-oldest. the function did the same when it
;;     encountered dup-previous: it disregarded dup-previous and retained dup-oldest.
;;     our final output here is '(1 3 2)
;;
;; however, this discrepancy will not be caught if the list to be passed through
;; either of the functions is already sorted. that is (sort '(2 1 2 3 2) <) will
;; yield us the same output when passed through either remove-duplicates-clone
;; or own-remove-duplicates:

(check-expect (equal? (own-remove-duplicates (sort '(2 1 2 3 2) <))
                      (remove-duplicates-clone (sort '(2 1 2 3 2) <)))
              #t)

;; *link to original remove-duplicates-clone: https://gist.github.com/leontastic/8502497

;;;; application

(test)
