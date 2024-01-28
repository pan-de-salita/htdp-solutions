#lang racket
(require test-engine/racket-tests)

;; an NEList-of-temperatures is one of:
;; - (cons Number '())
;; - (cons Number NEList-of-temperatures)

;; an NEList-of-Booleans is one of:
;; - (cons Boolean '())
;; - (cons Boolean NEList-of-Booleans)

;; an [NEList-of ITEM] is one of:
;; - (cons ITEM '())
;; - (cons ITEM [NEList-of ITEM])
