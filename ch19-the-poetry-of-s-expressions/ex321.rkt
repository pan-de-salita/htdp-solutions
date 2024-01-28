#lang racket
(require test-engine/racket-tests)

;; an S-expr is one of:
;; - Atom
;;   - any data type x that satisfies
;;     the predicate:
;;       (not (pair? x))
;; - SL
;;   - a [List-of S-expr]
