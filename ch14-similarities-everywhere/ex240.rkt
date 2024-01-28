#lang racket
(require test-engine/racket-tests)

(struct layer [stuff] #:transparent)
;; a layer is a structure:
;;   [layer ITEM]

;; an LStr is one of:
;; - String
;; - (layer LStr)
(define layer/str-0 "string")
(define layer/str-1 (layer layer/str-0))
(define layer/str-2 (layer layer/str-1))

;; an LNum is one of:
;; - Number
;; - (later LNum)
(define layer/num-0 0)
(define layer/num-1 (layer layer/num-0))
(define layer/num-2 (layer layer/num-1))

;; a [Layer-of ITEM] is one of:
;; - ITEM
;; (layer [Layer-of ITEM])

;; a [Layer-of String] is one of:
;; - String
;; - [layer [Layer-of String]]
;; in other words, an LStr.

;; a [Layer-of Number] is one of:
;; - Number
;; - [layer Layer-of Number]
;; in other words, an LNum.
