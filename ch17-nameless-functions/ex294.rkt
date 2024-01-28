#lang racket
(require test-engine/racket-tests)

;; [X] X [List-of X] -> [Maybe Number]
;; determines the index of the first occurrence
;; of x in l, #f otherwise

(check-expect (index 2 '()) #f)
(check-expect (index 2 '(2)) 0)
(check-expect (index 2 '(1 2 3)) 1)
(check-expect (index 2 '(-2 -1 0 1 2)) 4)
(check-expect (index 50 (append (make-list 1 1)
                                (make-list 2 2)
                                (make-list 50 44)
                                (make-list 3 30)
                                (make-list 3 50)
                                (make-list 5 100)))
              56)
(check-satisfied (index 10 '(10 20 30)) (is-index? 10 '(10 20 30)))
(check-satisfied (index 10 '(20 30 10)) (is-index? 10 '(20 30 10)))
(check-satisfied (index 50 (append (make-list 1 1)
                                   (make-list 2 2)
                                   (make-list 50 44)
                                   (make-list 3 30)
                                   (make-list 3 50)
                                   (make-list 5 100)))
                 (is-index? 50 (append (make-list 1 1)
                                       (make-list 2 2)
                                       (make-list 50 44)
                                       (make-list 3 30)
                                       (make-list 3 50)
                                       (make-list 5 100))))

(define (index x l)
  (cond [(empty? l) #f]
        [else (if (equal? x (car l))
                  0
                  (local ((define i (index x (cdr l))))
                    (if (boolean? i) i (add1 i))))]))

;; specification for index --------------------------------------------------

;; [X] X [List-of X] -> [[Maybe Number] -> Boolean]
;; is a specification for index, returns a function
;; that checks whether y is either:
;; - is #f or
;; - the index of the first occurrence of x in l

(check-expect [(is-index? 2 '()) #f] #f)
(check-expect [(is-index? 2 '(2)) 0] #t)
(check-expect [(is-index? 2 '(1 2 3)) 1] #t)
(check-expect [(is-index? 2 '(-2 -1 0 1 2)) 4] #t)
(check-expect [(is-index? 2 '(-2 -1 0 1 2 2 2)) 6] #f)
(check-expect [(is-index? 2 '(2 2 2 1 1)) 0] #t)
(check-expect [(is-index? 2 '(2 2 2 1 1)) 1] #f)
(check-expect [(is-index? 2 '(2 2 2 1 1)) 20] #f)
(check-expect [(is-index? 2 '(2 2 2 1 1)) #t] #f)
(check-expect [(is-index? 50 (append (make-list 1 1)
                                     (make-list 2 2)
                                     (make-list 50 44)
                                     (make-list 3 30)
                                     (make-list 3 50)
                                     (make-list 5 100)))
               56]
              #t)

(define (is-index? x l)
  (lambda (y)
    (local (;; [Maybe Boolean] -> Boolean
            ;; checks if x does NOT figure in l
            (define (x-not-found? maybe-boolean)
              (if (false? y)
                  (not (boolean? (member x l)))
                  #f))
            ;; [Maybe Number] -> Boolean
            ;; checks if maybe-index references the first occurrence of x in l
            (define (y==index? maybe-index)
              (cond [(not (number? maybe-index)) #f]
                    [else (and (not (boolean? (member x l)))
                               (if (not (< maybe-index (length l)))
                                   #f
                                   (and (equal? (l-ref l maybe-index) x)
                                        ;; checks if maybe-index references the first occurrance
                                        ;; of x in l by confirming that x does NOT figure in l indexed
                                        ;; from 0 up to (sub1 maybe-index). in other words, checks if
                                        ;; x does NOT figure in l indexed from 0 up to the point just
                                        ;; before maybe-index. if it does, then maybe-index does not
                                        ;; reference the first occurrence of x in l.
                                        ;;
                                        ;; for example, in '(2 2 2):
                                        ;;
                                        ;; - let maybe-index be 0
                                        ;; == #t because no occurrence of 2 figures before index 0
                                        ;; - let maybe-index be 1
                                        ;; == #f because an occurrence of 2 figures before index 1
                                        ;;
                                        ;; logic by YE: https://gitlab.com/cs-study/htdp/-/blob/main/03-Abstraction/17-Nameless-Functions/Exercise-294.rkt?ref_type=heads
                                        (false? (member x (build-list maybe-index (lambda (index) (l-ref l index))))))))])))
      (or (x-not-found? y)
          (y==index? y)))))

;; [List-of X] Number -> X
;; functions just like list-ref

(check-expect (l-ref '(1 2 3) 0) (list-ref '(1 2 3) 0))

(define (l-ref a-list an-index)
  (car [(apply-n-times cdr an-index) a-list]))

;; [X -> Y] Number -> [[List-of X] [List-of Y]]
;; returns a function that apples F n times to l

(check-expect [(apply-n-times cdr 0) '(1 2 3 4 5)] '(1 2 3 4 5))
(check-expect [(apply-n-times cdr 3) '(1 2 3 4 5)] '(4 5))
(check-expect (car [(apply-n-times cdr 3) '(1 2 3 4 5)]) 4)

(define (apply-n-times F n)
  (lambda (l)
    (if (zero? n)
        l
        (F [(apply-n-times F (sub1 n)) l]))))

;; [X] X [List-of X] -> [ [Maybe N] -> Boolean ]
;; specification for index; checks if test is one of:
;; - #f (meaning x was not found in l)
;; - the index of the first occurrence of x in l

(check-expect [(is-index?.v2 2 '()) #f] #t)
(check-expect [(is-index?.v2 2 '(2)) 0] #t)
(check-expect [(is-index?.v2 2 '(1 2 3)) 1] #t)
(check-expect [(is-index?.v2 2 '(-2 -1 0 1 2)) 4] #t)
(check-expect [(is-index?.v2 2 '(-2 -1 0 1 2 2 2)) 6] #f)
(check-expect [(is-index?.v2 2 '(2 2 2 1 1)) 0] #t)
(check-expect [(is-index?.v2 2 '(2 2 2 1 1)) 1] #f)
(check-expect [(is-index?.v2 2 '(2 2 2 1 1)) 20] #f)
(check-expect [(is-index?.v2 2 '(2 2 2 1 1)) #t] #f)
(check-expect [(is-index?.v2 50 (append (make-list 1 1)
                                     (make-list 2 2)
                                     (make-list 50 44)
                                     (make-list 3 30)
                                     (make-list 3 50)
                                     (make-list 5 100)))
               56]
              #t)

(define (is-index?.v2 x l)
  (lambda (test)
    (local (;; [Maybe Boolean] -> Boolean
            ;; checks whether x is not found in l
            (define (x-not-found? maybe-boolean)
              (if (false? maybe-boolean)
                  (false? (member x l))
                  #f))
            ;; [Maybe N] -> Boolean
            ;; checks whether maybe-index references the first
            ;; instance of x in l
            (define (index-of-first-x? maybe-index)
              (if (number? maybe-index)
                  (and (< maybe-index (length l))
                       (equal? (member x l)
                               [(apply-n-times cdr maybe-index) l])
                       ;; alternate logic by S8A:
                       ;; (= maybe-index (- (length l) (length (member x l))))
                       ;; assuming (member x l) does not return #f
                       )
                  #f)))
      (or (x-not-found? test)
          (index-of-first-x? test)))))

(test)
