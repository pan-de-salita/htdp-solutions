#lang racket
(require test-engine/racket-tests)

;; an S-expr is one of:
;; - Atom:
;;   -- Number
;;   -- String
;;   -- Symbol
;; - SL:
;;   -- '()
;;   -- (cons S-expr SL)

;; S-expr Symbol -> N
;; counts the number of occurrences of sy in sexp

(define (count sexp sy)
  (local (;; Atom -> N
          ;; adds 1 to the total count of sys within an sexp
          (define (count-atom atom)
            (if (and (symbol? atom) (symbol=? atom sy)) 1 0))
          ;; SL -> N
          ;; processes the items within an SL and sums the total
          ;; count of sys within an sexp
          (define (count-sl sl)
            (match sl
              [(? empty?) 0]
              [(? cons?)
               (+ (count (car sl) sy)
                  ;; according to the data definition of an
                  ;; SL, its first element is an S-expr, w/c
                  ;; is either an Atom or an SL. the appropriate
                  ;; function for processing this type of data
                  ;; is count because we don't know whether
                  ;; (car sl) is going to be an Atom or an SL.
                  (count-sl (cdr sl))
                  ;; on the other hand, the rest of an SL is
                  ;; by definition an SL. here we have an instance
                  ;; of self-reference, and therefore merits a
                  ;; recursive call through count-sl.
                  )])))
    (match sexp
      [(not (? pair?)) (count-atom sexp)]
      [(? list?) (count-sl sexp)])))

(check-expect (count 0 'hello) 0)
(check-expect (count "hello" 'hello) 0)
(check-expect (count 'hello 'hello) 1)
(check-expect (count '() 'hello) 0)
(check-expect (count '(0 "hello" hello) 'hello) 1)
(check-expect (count '(0 "hello" (hello (world hello))) 'hello) 2)

;; S-expr Symbol -> N
;; like count, counts the number of occurrences of sy in sexp

(define (count.v2 sexp sy)
  (local (;; Atom -> N
          ;; adds 1 to the total count of sys within an sexp
          (define (count-atom atom)
            (if (and (symbol? atom) (symbol=? atom sy)) 1 0))
          ;; SL -> N
          ;; processes the items within an SL and sums the total
          ;; count of sys within an sexp
          (define (count-sl sl)
            (foldr + 0 (map (curryr count.v2 sy) sl))))
    (match sexp
      [(not (? pair?)) (count-atom sexp)]
      [(? list?) (count-sl sexp)])))

(check-expect (count.v2 0 'hello) 0)
(check-expect (count.v2 "hello" 'hello) 0)
(check-expect (count.v2 'hello 'hello) 1)
(check-expect (count.v2 '() 'hello) 0)
(check-expect (count.v2 '(0 "hello" hello) 'hello) 1)
(check-expect (count.v2 '(0 "hello" (hello (world hello))) 'hello) 2)

(test)
