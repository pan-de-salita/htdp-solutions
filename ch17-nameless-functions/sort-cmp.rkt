#lang racket
(require test-engine/racket-tests)

;; [X] [list-of X] [X X -> Boolean] -> [List-of X]
;; sorts l according to cmp

(check-expect (sort-cmp '("c" "b") string<?) '("b" "c"))
(check-expect (sort-cmp '(2 1 3 4 6 5) <) '(1 2 3 4 5 6))

(define (sort-cmp l cmp)
  (cond [(empty? l) '()]
        [else (cond [(or (empty? (sort-cmp (cdr l) cmp))
                         (cmp (car l) (car (sort-cmp (cdr l) cmp))))
                     (cons (car l) (sort-cmp (cdr l) cmp))]
                    [else (sort-cmp (append (cdr l) (list (car l))) cmp)])]))

;; [X] [List-of X] [X X -> Boolean] -> [List-of X]
;; likes sort-cmp, sorts l according to cmp

(check-expect (sort-cmp-local '("c" "b") string<?) (sort-cmp '("c" "b") string<?))
(check-expect (sort-cmp-local '(2 1 3 4 6 5) <) (sort-cmp '(2 1 3 4 6 5) <))

(define (sort-cmp-local l cmp)
  (local (;; [List-of X] -> [List-of X]
          ;; sorts l using insertion sort
          (define (insertion-sort l)
            (cond [(empty? l) '()]
                  [else (insert (car l) (insertion-sort (cdr l)))]))
          ;; X [List-of X] -> [List-of X]
          ;; inserts x into a sorted list acording to cmp
          (define (insert x l-sorted)
            (cond [(empty? l-sorted) (list x)]
                  [else (cond [(cmp x (car l-sorted)) (cons x l-sorted)]
                              [else (cons (car l-sorted) (insert x (cdr l-sorted)))])])))
    (insertion-sort l)))

;; [X] [X X -> Boolean] -> [[List-of X] -> Boolean]
;; produces a function that determines whether some list is sorted according to cmp

(check-expect [(sorted string<?) (sort-cmp '("c" "b") string<?)] #t)
(check-expect [(sorted <) (sort-cmp '(2 1 3 4 6 5) <)] #t)

(check-satisfied (sort-cmp '("c" "b") string<?) (sorted string<?))
(check-satisfied (sort-cmp '(2 1 3 4 6 5) <) (sorted <))

(define (sorted cmp)
  (lambda (a-list)
    (local (;; [NEList-of X] -> Boolean
            ;; checks if l-x is sorted according to cmp
            (define (sorted? l)
              (cond [(empty? (cdr l)) #t]
                    [else (and (cmp (car l) (cadr l))
                               (sorted? (cdr l)))])))
      (if (empty? a-list) #t (sorted? a-list)))))

;; [X] [List-of X] [X X -> Boolean] -> [[List-of X] -> Boolean]
;; like sorted, produces a function that determines whether
;; - some list is sorted according to cmp
;; - some list is a variant of a reference list

(check-expect [(sorted.v2 '("b" "c") string<?) (sort-cmp '("c" "b") string<?)] #t)
(check-expect [(sorted.v2 '(2 1 3 4 6 5) <) (sort-cmp '(2 1 3 4 6 5) <)] #t)

(check-satisfied (sort-cmp '("c" "b") string<?) (sorted.v2 '("b" "c") string<?))
(check-satisfied (sort-cmp '(2 1 3 4 6 5) <) (sorted.v2 '(2 1 3 4 6 5) <))

(define (sorted.v2 l-ref cmp)
  (lambda (a-list)
    (local (;; [NEList-of X] -> Boolean
            ;; checks if l-x is sorted according to cmp
            (define (sorted? l)
              (cond [(empty? (cdr l)) #t]
                    [else (and (cmp (car l) (cadr l))
                               (sorted? (cdr l)))]))
            ;; [List-of X] [List-of X] -> Boolean
            ;; checks if l-x contains all elements of l-y
            (define (contains? l-x l-y)
              (andmap (lambda (from-l-x) (not (boolean? (member from-l-x l-y)))) l-x)))
      (if (empty? a-list)
          #t
          (and (sorted? a-list)
               (contains? a-list l-ref)
               (contains? l-ref a-list))))))

(define a-list
  (build-list 5 (lambda (x) (random 5))))

(check-satisfied (sort-cmp a-list <=) (sorted.v2 a-list <=))

(test)

;; will return to this:

;; [X] [List-of X] [X X -> Boolean] -> [List-of X]
;; likes sort-cmp, sorts l according to cmp

;; (check-expect (sort-cmp-lambda '("c" "b") string<?) (sort-cmp '("c" "b") string<?))
;; (check-expect (sort-cmp-lambda '(2 1 3 4 6 5) <) (sort-cmp '(2 1 3 4 6 5) <)) ;; fails
;; (check-expect (sort-cmp-lambda '(2 1 3) <) (sort-cmp '(2 1 3) <))

;; (define (sort-cmp-lambda l cmp)
;;   (foldr (lambda (from-l l-occurring-0)
;;            (cond []
;;                  []))
;;          '()
;;          l))
