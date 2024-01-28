#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define-struct centry [name home office cell])
;; laws added
;; (centry-name (make-centry name home office cell)) == name
;; (centry-home (make-centry name home office cell)) == home
;; (centry-office (make-centry name home office cell)) == office
;; (centry-cell (make-centry name home office cell)) == cell

(define-struct phone [area number])
;; laws added
;; (phone-area (make-phone area number)) == area
;; (phone-number (make-phone area number)) == number
