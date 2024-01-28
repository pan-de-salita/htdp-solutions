#lang racket
(require test-engine/racket-tests)

(define-struct phone [area switch four] #:transparent)

(define l-phone0-input (list (make-phone 713 664 9993)))
(define l-phone1-input
  (list (make-phone 713 123 1234)
        (make-phone 713 456 5678)
        (make-phone 713 789 9101)))
(define l-phone2-input
  (list (make-phone 713 123 1234)
        (make-phone 222 000 0000)
        (make-phone 713 456 5678)
        (make-phone 222 111 1111)
        (make-phone 713 789 9101)))

(define l-phone0-output (list (make-phone 281 664 9993)))
(define l-phone1-output
  (list (make-phone 281 123 1234)
        (make-phone 281 456 5678)
        (make-phone 281 789 9101)))
(define l-phone2-output
  (list (make-phone 281 123 1234)
        (make-phone 222 000 0000)
        (make-phone 281 456 5678)
        (make-phone 222 111 1111)
        (make-phone 281 789 9101)))

;; [List-of Phone] -> [List-of Phone]
;; substitutes the area code 713 with 281 in a list
;; of phone records

(check-expect (replace l-phone0-input) l-phone0-output)
(check-expect (replace l-phone1-input) l-phone1-output)
(check-expect (replace l-phone2-input) l-phone2-output)

(define (replace l-phone)
  (for/list ([a-phone l-phone])
    (match a-phone
      [(phone 713 switch four) (make-phone 281 switch four)]
      [(phone area switch four) (make-phone area switch four)])))

(test)
