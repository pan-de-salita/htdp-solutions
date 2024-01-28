#lang racket
(require test-engine/racket-tests)

;;;; attempt 2 of general list-sorting functions.

;; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
;; sorts a list of numbers according to comparator f.

(check-expect (sort-num '() >) '())
(check-expect (sort-num (list 1 2) >) (list 2 1))
(check-expect (sort-num (list 1 2) <) (list 1 2))
(check-expect (sort-num (list 1 2 3) >) (list 3 2 1))
(check-expect (sort-num (list 1 2 3) <) (list 1 2 3))
(check-expect (sort-num (list 2 1 3) >) (list 3 2 1))
(check-expect (sort-num (list 2 1 3) <) (list 1 2 3))
(check-expect (sort-num (list 14 56 2 38) >) (list 56 38 14 2))
(check-expect (sort-num (list 14 56 2 38) <) (list 2 14 38 56))

(define (sort-num l-num f)
  (cond [(empty? l-num) '()]
        [else (cond [(or (empty? (sort-num (cdr l-num) f))
                         (f (car l-num) (car (sort-num (cdr l-num) f))))
                     (cons (car l-num) (sort-num (cdr l-num) f))]
                    [else (sort-num (append (cdr l-num) (list (car l-num))) f)])]))

;; [List-of String] [String String -> Boolean] -> [List-of String]
;; sorts a list of strings according to comparator f.

(check-expect (sort-str '() string>?) '())
(check-expect (sort-str (list "a") string>?) (list "a"))
(check-expect (sort-str (list "a" "b") string>?) (list "b" "a"))
(check-expect (sort-str (list "a" "b") string<?) (list "a" "b"))
(check-expect (sort-str (list "a" "b" "c") string>?) (list "c" "b" "a"))
(check-expect (sort-str (list "a" "b" "c") string<?) (list "a" "b" "c"))
(check-expect (sort-str (list "c" "a" "b") string>?) (list "c" "b" "a"))
(check-expect (sort-str (list "c" "a" "b") string<?) (list "a" "b" "c"))
(check-expect (sort-str (list "z" "a" "p" "d") string>?) (list "z" "p" "d" "a"))
(check-expect (sort-str (list "z" "a" "p" "d") string<?) (list "a" "d" "p" "z"))

(define (sort-str l-str f)
  (cond [(empty? l-str) '()]
        [else (cond [(or (empty? (sort-str (cdr l-str) f))
                         (f (car l-str) (car (sort-str (cdr l-str) f))))
                     (cons (car l-str) (sort-str (cdr l-str) f))]
                    [else (sort-str (append (cdr l-str) (list (car l-str))) f)])]))

;; [X] [List-of X] [X X -> Boolean] -> [List-of X]
;; sorts a list of elements of the same type according to comparator f.
;; strategy: given a list '(x-1 x-2 ... x-n),
;; if (cmp x-1 (sort-l (cdr '(x-1 x-2 ... x-n)) cmp)) holds true
;; or (sort-l (cdr '(x-1 x-2 ... x-n)) cmp) produces an empty list,
;; cons x-1 (sort-l (cdr '(x-1 x-2 ... x-n)) cmp). NOTE: consing takes place only if elements are in sorted order.
;; else, make new list by appending x-1 to '(x-1 x-2 ... x-n)
;; and then pass it to sort-l. NOTE: if elements are not in order, the elements are reordered.

(check-expect (sort-l (list 14 56 2 38) >) (sort-num (list 14 56 2 38) >))
(check-expect (sort-l (list 14 56 2 38) <) (sort-num (list 14 56 2 38) <))
(check-expect (sort-l (list "z" "a" "p" "d") string>?) (sort-str (list "z" "a" "p" "d") string>?))
(check-expect (sort-l (list "z" "a" "p" "d") string<?) (sort-str (list "z" "a" "p" "d") string<?))

(define (sort-l l f)
  (cond [(empty? l) '()]
        [else (cond [(or (empty? (sort-l (cdr l) f))
                         ;; checks if the next recursion produces an empty list.
                         ;; NOTE: if and was used instead of or, an error is
                         ;; returned because f cannot campare an element with
                         ;; an empty element. another NOTE: if the following is
                         ;; used:
                         ;;
                         ;;   (and (cons? (sort-l (cdr l) f))
                         ;;        (f (car l) (car (sort-l (cdr l) f))))
                         ;;
                         ;; the function enters an infinite loop because once the
                         ;; cons? clause returns #f, sort-l  would be applied to a
                         ;; new list with the empty list preppended to the last non-empty
                         ;; element. this new list would then be evaluated by the cons?
                         ;; clause again, inevitably reaching an empty list and resulting
                         ;; in #f once evaluated by the cons clause, in turn leading to
                         ;; sort-l being applied to the same "new" list with the empty
                         ;; list preppended to the last non-empty element again. this
                         ;; entire process repeats with no end in sight. see end for
                         ;; sample computation with sort-l-infinite-loop.
                         (f (car l) (car (sort-l (cdr l) f))))
                         ;; compares the first element of l to the first element
                         ;; of a sorted list of elements. if true:
                     (cons (car l) (sort-l (cdr l) f))]
                     ;; conses the first element of l to a sorted list of
                     ;; elements. NOTE: the list to be sorted is made
                     ;; smaller and can begin to approach the base case.
                    [else (sort-l (append (cdr l) (list (car l))) f)])]))
                    ;; reorders the list to be sorted by placing its first
                    ;; element at the end of it. NOTE: the size of the list
                    ;; to be sorted remains the same.

(struct book [title price] #:transparent)
;; an Book is a structure:
;;   (Inventory String Number)
;; an book's title and price.
(define htdp (book "htdp" 60))
(define tls (book "the little schemer" 34))
(define sicp (book "sicp" 50))
(define tss (book "the seasoned schemer" 34))
(define books-to-buy `(,htdp ,tls ,sicp ,tss))

;; [List-of Book] [Book Book -> Boolean] -> [List-of Book]
;; sorts a list of Books alphabetically according to comparator f.

(check-expect
 (sort-book books-to-buy sort-book-title/>)
 `(,tss ,tls ,sicp ,htdp))
(check-expect
 (sort-book books-to-buy sort-book-title/<)
 `(,htdp ,sicp ,tls ,tss))
(check-expect
 (sort-book books-to-buy sort-book-price/>)
 `(,htdp ,sicp ,tss ,tls))
(check-expect
 (sort-book books-to-buy sort-book-price/<)
 `(,tls ,tss ,sicp ,htdp))

(define (sort-book l-book f)
  (sort-l l-book f))

;; Book Book -> Boolean
;; checks whether two book titles are descending.

(check-expect (sort-book-title/> htdp sicp) #f)
(check-expect (sort-book-title/> sicp htdp) #t)

(define (sort-book-title/> book1 book2)
  (string>=? (book-title book1) (book-title book2)))

;; Book Book -> Boolean
;; checks whether two book titles are nondescending.

(check-expect (sort-book-title/< htdp sicp) #t)
(check-expect (sort-book-title/< sicp htdp) #f)

(define (sort-book-title/< book1 book2)
  (string<=? (book-title book1) (book-title book2)))

;; Book Book -> Boolean
;; checks whether two book prices are descending.

(check-expect (sort-book-price/> htdp sicp) #t)
(check-expect (sort-book-price/> sicp htdp) #f)

(define (sort-book-price/> book1 book2)
  (>= (book-price book1) (book-price book2)))

;; Book Book -> Boolean
;; checks whether two book prices are nondescending.

(check-expect (sort-book-price/< htdp sicp) #f)
(check-expect (sort-book-price/< sicp htdp) #t)

(define (sort-book-price/< book1 book2)
  (<= (book-price book1) (book-price book2)))

;;;; attempt 1 of general list-sorting functions.

;; [List-of Number] [Number Number -> Boolean] -> [List-of Number]
;; produces a sorted list of numbers.

(check-expect (sort-n (list 1 2) >) (list 2 1))
(check-expect (sort-n (list 1 2 3) >) (list 3 2 1))
(check-expect (sort-n (list 2 1) <) (list 1 2))
(check-expect (sort-n (list 3 2 1) <) (list 1 2 3))
(check-expect (sort-n (list 2 1 3) >) (list 3 2 1))
(check-expect (sort-n (list 2 1 3) <) (list 1 2 3))

(define (sort-n l-n F)
  (cond [(empty? l-n) '()]
        [else (insert-n (car l-n) (sort-n (cdr l-n) F) F)]))

;; Number [List-of Number] Function -> [List-of Number]
;; inserts x into a sorted list of numbers l.

(check-expect (insert-n 1 (list 3 2) >) (list 3 2 1))
(check-expect (insert-n 1 (list 2 3) <) (list 1 2 3))

(define (insert-n x l-n F)
  (cond [(empty? l-n) (list x)]
        [else (cond [(F x (car l-n)) (cons x l-n)]
                    [else (cons (car l-n) (insert-n x (cdr l-n) F))])]))

(test)

;;;; sample error computation of sort-l:

;; sort-l with:
;;   (and (cons? (sort-l (cdr l) f))
;;        (f (car l) (car (sort-l (cdr l) f))))

(define (sort-l-infinite-loop l cmp)
  (cond [(empty? l) '()]
        [else (cond [(and (cons? (sort-l-infinite-loop (cdr l) cmp))
                          (cmp (car l) (car (sort-l-infinite-loop (cdr l) cmp))))
                     (cons (car l) (sort-l-infinite-loop (cdr l) cmp))]
                    [else (sort-l-infinite-loop (append (cdr l) (list (car l))) cmp)])]))

#|
(sort-l-infinite-loop (list 2 1 3) >)
==
(cond [(empty? (list 2 1 3)) '()]
      [else (cond [(and (cons? (sort-l-infinite-loop (cdr (list 2 1 3)) >))
                        (> (car (list 2 1 3)) (car (sort-l-infinite-loop (cdr (list 2 1 3)) >))))
                   (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
                  [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])])
==
(cond [#f '()]
      [else (cond [(and (cons? (sort-l-infinite-loop (cdr (list 2 1 3)) >))
                        (> (car (list 2 1 3)) (car (sort-l-infinite-loop (cdr (list 2 1 3)) >))))
                   (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
                  [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])])
==
(cond [else (cond [(and (cons? (sort-l-infinite-loop (cdr (list 2 1 3)) >))
                        (> (car (list 2 1 3)) (car (sort-l-infinite-loop (cdr (list 2 1 3)) >))))
                   (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
                  [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])])
==
(cond [(and (cons? (sort-l-infinite-loop (cdr (list 2 1 3)) >))
            (> (car (list 2 1 3)) (car (sort-l-infinite-loop (cdr (list 2 1 3)) >))))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons? (sort-l-infinite-loop (list 1 3) >))
            (> (car (list 2 1 3)) (car (sort-l-infinite-loop (cdr (list 2 1 3)) >))))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(empty? (list 1 3)) '()]
                   [else (cond [(and (cons? (sort-l-inifinite-loop (cdr (list 1 3)) >))
                                     (> (car (list 1 3)) (car (sort-l-inifinite-loop (cdr (list 1 3)) >))))
                                (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                               [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)])]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [#f '()]
                   [else (cond [(and (cons? (sort-l-inifinite-loop (cdr (list 1 3)) >))
                                     (> (car (list 1 3)) (car (sort-l-inifinite-loop (cdr (list 1 3)) >))))
                                (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                               [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)])]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [else (cond [(and (cons? (sort-l-inifinite-loop (cdr (list 1 3)) >))
                                     (> (car (list 1 3)) (car (sort-l-inifinite-loop (cdr (list 1 3)) >))))
                                (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                               [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)])]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons? (sort-l-inifinite-loop (cdr (list 1 3)) >))
                         (> (car (list 1 3)) (car (sort-l-inifinite-loop (cdr (list 1 3)) >))))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons? (sort-l-inifinite-loop (list 3) >)) ;; NOTE: the infinite loop repeats this step.
                         (> (car (list 1 3)) (car (sort-l-inifinite-loop (cdr (list 1 3)) >))))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [(empty? (list 3)) '()]
                                [else (cond [(and (cons? (sort-l-infinite-loop (cdr (list 3)) >))
                                                  (> (car (list 3)) (car (sort-l-infinite-loop (cdr (list 3)) >))))
                                             (cons (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))]
                                            [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)])]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [#f '()]
                                [else (cond [(and (cons? (sort-l-infinite-loop (cdr (list 3)) >))
                                                  (> (car (list 3)) (car (sort-l-infinite-loop (cdr (list 3)) >))))
                                             (cons (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))]
                                            [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)])]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [else (cond [(and (cons? (sort-l-infinite-loop (cdr (list 3)) >))
                                                  (> (car (list 3)) (car (sort-l-infinite-loop (cdr (list 3)) >))))
                                             (cons (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))]
                                            [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)])]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [(and (cons? (sort-l-infinite-loop (cdr (list 3)) >))
                                      (> (car (list 3)) (car (sort-l-infinite-loop (cdr (list 3)) >))))
                                 (cons (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))]
                                [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [(and (cons?
                                       (cond [(empty? '()) '()]
                                             [else (cond [(and (cons? (sort-l-infinite-loop (cdr '()) >))
                                                               (> (car '()) (car (sort-l-infinite-loop (cdr '()) >))))
                                                          (cons (car '()) (sort-l-infinite-loop (cdr '()) >))]
                                                         [else (sort-l-infinite-loop (append (cdr '()) (list (cdr '()))) >)])]))
                                      (> (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >)))
                                 (cons (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))]
                                [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [(and (cons?
                                       (cond [#t '()]
                                             [else (cond [(and (cons? (sort-l-infinite-loop (cdr '()) >))
                                                               (> (car '()) (car (sort-l-infinite-loop (cdr '()) >))))
                                                          (cons (car '()) (sort-l-infinite-loop (cdr '()) >))]
                                                         [else (sort-l-infinite-loop (append (cdr '()) (list (cdr '()))) >)])]))
                                      (> (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >)))
                                 (cons (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))]
                                [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [(and (cons? '()
                                      (> (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))))
                                 (cons (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))]
                                [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [(and (#f
                                      (> (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))))
                                 (cons (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))]
                                [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [#f
                                 (cons (car (list 3)) (sort-l-infinite-loop (cdr (list 3)) >))]
                                [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (cond [else (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >)]))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (sort-l-infinite-loop (append (cdr (list 3)) (list (car (list 3)))) >))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (sort-l-infinite-loop (append '() (list (car (list 3)))) >))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (sort-l-infinite-loop (append '() (list 3)) >))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])
==
(cond [(and (cons?
             (cond [(and (cons?
                          (sort-l-infinite-loop (list 3) >))
                         (> (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >)))
                    (cons (car (list 1 3)) (sort-l-inifinite-loop (cdr (list 1 3)) >))]
                   [else (sort-l-infinite-loop (append (cdr (list 1 3)) (list (car (list 1 3)))) >)]))
            (> (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >)))
       (cons (car (list 2 1 3)) (sort-l-infinite-loop (cdr (list 2 1 3)) >))]
      [else (sort-l-infinite-loop (append (cdr (list 2 1 3)) (list (car (list 2 1 3)))) >)])

|#
