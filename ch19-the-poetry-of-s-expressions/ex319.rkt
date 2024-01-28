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

;; S-expr Symbol Symbol -> S-expr
;; substitutes all occurrences of old with replacement
;; in sexp

(define (substitute sexp old replacement)
  (local (;; Atom -> Atom
          (define (substitute-atom atom)
            (if (and (symbol? atom) (symbol=? atom old)) replacement atom))
          ;; SL -> SL
          (define (substitute-sl sl)
            (match sl
              [(? null?) null]
              [(? cons?)
               (cons (substitute (car sl) old replacement)
                     (substitute-sl (cdr sl)))])))
    (match sexp
      [(not (? pair?)) (substitute-atom sexp)]
      [(? list?) (substitute-sl sexp)])))

(check-expect (substitute 0 'zero 'one) 0)
(check-expect (substitute "zero" 'zero 'one) "zero")
(check-expect (substitute 'zero 'zero 'one) 'one)
(check-expect (substitute '() 'zero 'one) '())
(check-expect (substitute '(zero zero zero) 'zero 'one) '(one one one))
(check-expect (substitute '(zero ((zero)) ((zero ("zero")) zero)) 'zero 'one) '(one ((one)) ((one ("zero")) one)))

;; S-expr Symbol Symbol -> S-expr
;; like substitute, substitutes all occurrences of old with
;; replacement (here represented as new) in sexp

(define (substitute.v2 sexp old new)
  (match sexp
    [(not (? pair?))
     (if (and (symbol? sexp)
              (symbol=? sexp old))
         new
         sexp)]
    [(? list?)
     (map (curryr substitute.v2 old new) sexp)]))

(check-expect (substitute.v2 0 'zero 'one) 0)
(check-expect (substitute.v2 "zero" 'zero 'one) "zero")
(check-expect (substitute.v2 'zero 'zero 'one) 'one)
(check-expect (substitute.v2 '() 'zero 'one) '())
(check-expect (substitute.v2 '(zero zero zero) 'zero 'one) '(one one one))
(check-expect (substitute.v2 '(zero ((zero)) ((zero ("zero")) zero)) 'zero 'one) '(one ((one)) ((one ("zero")) one)))

(test)
