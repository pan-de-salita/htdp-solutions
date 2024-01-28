#lang racket
(require test-engine/racket-tests)


;; [X X -> Boolean] -> [[List-of X] -> Boolean]
;; produces a function that determines whether
;; some list is sorted according to cmp

(check-expect [(sorted string<?) '("b" "c")] #t)
(check-expect [(sorted <) '(2 1 3 4 6 5)] #f)
(check-expect [(sorted <) '(1 2 3 4 5 6)] #t)

(define (sorted cmp)
  (lambda (applied-list)
    (local (;; [X X -> Boolean] [NEList-of X] -> Boolean
            ;; determines whether l is sorted according to cmp
            (define (sorted? l)
              (cond [(empty? (cdr l)) #t]
                    [else (and (cmp (car l) (cadr l))
                               (sorted? (cdr l)))])))
      (if (empty? applied-list) #t (sorted? applied-list)))))

;; [X X -> Boolean] -> [[List-of X] -> Boolean]
;; like sorted, produces a function that determines whether
;; some list is sorted according to cmp

(check-expect [(sorted.v2 string<?) '("b" "c")] #t)
(check-expect [(sorted.v2 <) '(2 1 3 4 6 5)] #f)
(check-expect [(sorted.v2 <) '(1 2 3 4 5 6)] #t)

(define (sorted.v2 cmp)
  (lambda (l)
    (cond [(or (empty? l) (empty? (cdr l))) #t]
          [else (and (cmp (car l) (cadr l))
                     [(sorted.v2 cmp) (cdr l)])])))

;; NOTE: cmp is not consumed by sorted? because the
;; function is locally defined. locally-defined functions
;; have access to the variables within the scope of
;; its main function. to specify that sorted? needs
;; to consume cmp may be redundant.

;;;; application

(check-satisfied (sort '("b" "c") string<?) (sorted string<?))
(check-satisfied (sort '(2 1 3 4 6 5) <) (sorted <))
(check-satisfied (sort '(1 2 3 4 5 6) <) (sorted <))
(check-satisfied (sort '("b" "c") string<?) (sorted.v2 string<?))
(check-satisfied (sort '(2 1 3 4 6 5) <) (sorted.v2 <))
(check-satisfied (sort '(1 2 3 4 5 6) <) (sorted.v2 <))

(test)
