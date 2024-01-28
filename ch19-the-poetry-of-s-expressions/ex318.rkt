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

;; S-expr -> Number
;; determines the depth of an S-expr:
;; - an Atom has a depth of 1
;; - the depth of an SL is the maximum depth of its items plus 1
;;   in other words, the level of nestedness of a given SL plus 1

(define (depth sexp)
  (local (;; Atom -> Number
          ;; determines the depth of an Atom
          (define (depth-atom atom) 1)
          ;; SL -> Number
          ;; determines the depth of an SL
          (define (depth-sl sl)
            (match sl
              [(? null?) 1]
              [(? cons?)
               (own-max (add1 (depth (car sl)))
                        ;; since (car sl) can either be an Atom or an SL,
                        ;; we use depth to determine its depth.
                        ;;
                        ;; (car sl) as Atom:
                        ;; if (car sl) is an Atom, it is eventually evaluated
                        ;; via depth-atom by way of depth, w/c returns a depth
                        ;; of 2 (1 for the Atom itself, and 1 from the add1
                        ;; called after depth evaluates the Atom (the add1 here
                        ;; referring to the list wrapping around the Atom)).
                        ;;
                        ;; (car sl) as SL:
                        ;; if (car sl) is an SL, it will be continually
                        ;; evaluated until (null? sl) returns #t. the depth of
                        ;; (car sl) as an SL is computed via each application of
                        ;; add1 as (car al) is passed through depth and eventually
                        ;; depth-sl. this is so because add1 is applied every time
                        ;; a nested list is detected and can be broken down. in other
                        ;; words, add1 counts the nestedness of an SL, terminating
                        ;; only when an Atom is recognized.
                        ;;
                        ;; once we have the depth of all SLs in our original S-expr,
                        ;; the program selects the largest depth as the overall depth.
                        ;; the largest depth is the correct answer because it is the
                        ;; accumulation of all nested SLs plus 1. for example,
                        ;; '(layer-1 (layer-2 (layer 3))) would have the depth of 4.
                        ;; the calculation:
                        ;; - the sub-SL holding 'layer-1 has a depth of 2
                        ;;   (1 plus the depth of the Atom 'layer-1)
                        ;; - the sub-SL holding 'layer-2 has a depth of 1 + 2
                        ;;   (1 plus the depth of the sub-SL holding 'layer-1)
                        ;; - the sub-SL holding 'layer-3 has a depth of 1 + 1 + 2
                        ;;   (1 plus the depth of the sub-SL holding 'layer-2
                        ;;   plus the depth of the sub-SL holding 'layer-1)
                        ;; since the depth of of the sub-SL holding 'layer-3 is the
                        ;; accumulation of all other sub-SLs, it is the depth of the
                        ;; overall S-expr.
                        (depth-sl (cdr sl))
                        ;; checks the next item on an SL
                        )]))
          ;; Number ...+ -> Number
          ;; like max, returns the largest of the ns provided
          (define (own-max n . ns)
            (foldr (lambda (x y) (if (>= x y) x y)) 0 (cons n ns))))
    (match sexp
      [(not (? pair?)) (depth-atom sexp)]
      [(? list?) (depth-sl sexp)])))

(check-expect (depth 0) 1)
(check-expect (depth "hello") 1)
(check-expect (depth 'world) 1)
(check-expect (depth '()) 1)
(check-expect (depth '(0 "hello" world)) 2)
(check-expect (depth '(0 "hello" (world))) 3)
(check-expect (depth '(0 "hello" (world (("!"))))) 5)
(check-expect (depth '(a b (c d (e (f ((((((g-is-the-shiest-letter))))))))))) 11)
(check-expect (depth '(((world) hello) hello)) 4)

;; S-expr -> Number
;; like depth, determines the depth of an S-expr:
;; - an Atom has a depth of 1
;; - the depth of an SL is the maximum depth of its items plus 1
;;   in other words, the level of nestedness of a given SL plus 1
;; NOTE: logic from S8A:
;;   https://github.com/S8A/htdp-exercises/blob/master/ex318.rkt

(define (depth.v2 sexp)
  (local (;; Atom -> Number
          ;; determines the depth of an Atom
          (define (depth.v2-atom atom) 1)
          ;; SL -> Number
          ;; determines the depth of an SL
          (define (depth.v2-sl sl)
            (add1 (foldr max 0 (map depth.v2 sl)))))
    (match sexp
      [(not (? pair?)) (depth.v2-atom sexp)]
      [(? list?) (depth.v2-sl sexp)])))

(check-expect (depth.v2 0) 1)
(check-expect (depth.v2 "hello") 1)
(check-expect (depth.v2 'world) 1)
(check-expect (depth.v2 '()) 1)
(check-expect (depth.v2 '(0 "hello" world)) 2)
(check-expect (depth.v2 '(0 "hello" (world))) 3)
(check-expect (depth.v2 '(0 "hello" (world (("!"))))) 5)
(check-expect (depth.v2 '(a b (c d (e (f ((((((g-is-the-shiest-letter))))))))))) 11)
(check-expect (depth.v2 '(((world) hello) hello)) 4)

(test)
