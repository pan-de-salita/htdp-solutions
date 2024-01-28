#lang racket
(require test-engine/racket-tests)

;; [List-of 1String] -> [List-of [List-of 1String]]
;; produces a list of all prefixes of l-1string

(check-expect (prefixes '()) '())
(check-expect (prefixes '("a")) '(("a")))
(check-expect (prefixes '("a" "b")) '(("a" "b") ("a")))
(check-expect (prefixes '("a" "b" "c")) '(("a" "b" "c") ("a" "b") ("a")))
(check-expect (prefixes '("a" "a" "b" "b" "c" "c" "d"))
              '(("a" "a" "b" "b" "c" "c" "d")
                ("a" "a" "b" "b" "c" "c")
                ("a" "a" "b" "b" "c")
                ("a" "a" "b" "b")
                ("a" "a" "b")
                ("a" "a")
                ("a")))

(define (prefixes l-1string)
  (cond [(empty? l-1string) '()]
        [else (cons l-1string (prefixes (reverse (cdr (reverse l-1string)))))]))

;; [List-of 1String] -> [List-of [List-of 1String]]
;; produces a list of all suffices of l-1string

(check-expect (suffixes '()) '())
(check-expect (suffixes '("a")) '(("a")))
(check-expect (suffixes '("a" "b")) '(("a" "b") ("b")))
(check-expect (suffixes '("a" "b" "c")) '(("a" "b" "c") ("b" "c") ("c")))
(check-expect (suffixes '("a" "a" "b" "b" "c" "c" "d"))
              '(("a" "a" "b" "b" "c" "c" "d")
                ("a" "b" "b" "c" "c" "d")
                ("b" "b" "c" "c" "d")
                ("b" "c" "c" "d")
                ("c" "c" "d")
                ("c" "d")
                ("d")))

(define (suffixes l-1string)
  (cond [(empty? l-1string) '()]
        [else (cons l-1string (suffixes (cdr l-1string)))]))

;; [List-of X] -> [List-of [List-of X]]
;; prefixes-abstracted works just like prefixes

(check-expect (prefixes-abstracted '()) (prefixes '()))
(check-expect (prefixes-abstracted '("a")) (prefixes '("a")))
(check-expect (prefixes-abstracted '("a" "b")) (prefixes '("a" "b")))
(check-expect (prefixes-abstracted '("a" "b" "c")) (prefixes '("a" "b" "c")))
(check-expect (prefixes-abstracted '("a" "a" "b" "b" "c" "c" "d")) (prefixes '("a" "a" "b" "b" "c" "c" "d")))

(define (prefixes-abstracted l)
  (local (;; Number -> [List-of X]
          ;; creates a prefix of l of length (- (length l) n)
          (define (nth-prefix n)
            (local (;; Number [List-of X] -> [List-of X]
                    ;; removes n items from l starting from its last item
                    (define (remove-from-end l index)
                      (cond [(zero? index) l]
                            [else (remove-from-end (reverse (cdr (reverse l))) (sub1 index))])))
              (remove-from-end l n))))
    (build-list (length l) nth-prefix)))

;; [List-of X] -> [List-of [List-of X]]
;; suffixes-abstracted works just like suffixes

(check-expect (suffixes-abstracted '()) (suffixes '()))
(check-expect (suffixes-abstracted '("a")) (suffixes '("a")))
(check-expect (suffixes-abstracted '("a" "b")) (suffixes '("a" "b")))
(check-expect (suffixes-abstracted '("a" "b" "c")) (suffixes '("a" "b" "c")))
(check-expect (suffixes-abstracted '("a" "a" "b" "b" "c" "c" "d")) (suffixes '("a" "a" "b" "b" "c" "c" "d")))

(define (suffixes-abstracted l)
  (local (;; Number -> [List-of X]
          ;; creates a suffix of l of length (- (length l) n)
          (define (nth-suffix n)
            (local (;; Number [List-of X] -> [List-of X]
                    ;; removes n items from l starting from its first item
                    (define (remove-from-start l end-index)
                      (cond [(zero? end-index) l]
                            [else (remove-from-start (cdr l) (sub1 end-index))])))
              (remove-from-start l n))))
    (build-list (length l) nth-suffix)))

;;;; further abstraction by S8A
;;;;   https://github.com/S8A/htdp-exercises/blob/master/ex190.rkt

;; [List-of 1String] -> [List-of [List-of 1String]]
;; prefixes-abstracted.v2 works just like prefixes

(check-expect (prefixes-abstracted.v2 '()) (prefixes '()))
(check-expect (prefixes-abstracted.v2 '("a")) (prefixes '("a")))
(check-expect (prefixes-abstracted.v2 '("a" "b")) (prefixes '("a" "b")))
(check-expect (prefixes-abstracted.v2 '("a" "b" "c")) (prefixes '("a" "b" "c")))
(check-expect (prefixes-abstracted.v2 '("a" "a" "b" "b" "c" "c" "d")) (prefixes '("a" "a" "b" "b" "c" "c" "d")))

(define (prefixes-abstracted.v2 l-1string)
  (local (;; N -> [List-of 1String]
          ;; removes n items from l-1string starting from its last item
          (define (remove-from-end n)
            (cond [(zero? n) l-1string]
                  [else (reverse (cdr (reverse (remove-from-end (sub1 n)))))])))
    (build-list (length l-1string) remove-from-end)))

;; [List-of 1String] -> [List-of [List-of 1String]]
;; suffixes-abstracted.v2 works just like suffixes

(check-expect (suffixes-abstracted.v2 '()) (suffixes '()))
(check-expect (suffixes-abstracted.v2 '("a")) (suffixes '("a")))
(check-expect (suffixes-abstracted.v2 '("a" "b")) (suffixes '("a" "b")))
(check-expect (suffixes-abstracted.v2 '("a" "b" "c")) (suffixes '("a" "b" "c")))
(check-expect (suffixes-abstracted.v2 '("a" "a" "b" "b" "c" "c" "d")) (suffixes '("a" "a" "b" "b" "c" "c" "d")))

(define (suffixes-abstracted.v2 l-1string)
  (local (;; N -> [List-of 1String]
          ;; removes n items from l-1string starting from its first item
          (define (remove-from-start n)
            (cond [(zero? n) l-1string]
                  [else (cdr (remove-from-start (sub1 n)))])))
    (build-list (length l-1string) remove-from-start)))

(test)
