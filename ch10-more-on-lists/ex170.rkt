#lang racket
(require test-engine/racket-tests)
(require lang/posn)

;;; CONSTANTS ----------------------------------------------

;;; DATA DEFINITIONS ---------------------------------------

(define-struct phone [area switch four])
;; a Phone is a structure:
;;     (make-phone Three Three Four)
;; a Three is a Number between 100 and 999
;; a Four is a Number between 1000 and 9999

;; a List-of-Phones is one of:
;; - '()
;; - (cons Phone List-of-Phone)
(define LOP-EX0 '())
(define LOP-EX1
  (cons (make-phone 713 123 4567) '()))
(define LOP-EX2
  (cons (make-phone 808 987 6543)
        (cons (make-phone 281 321 0123)
              (cons (make-phone 713 123 4567) '()))))

;;; FUNCTIONS ----------------------------------------------

;; List-of-Phones -> List-of-Phones
;; replaces all occurance of area code 713 with 281 in a
;; List-of-Phones lop

(check-expect (replace LOP-EX0) '())
(check-expect (replace LOP-EX1)
              (cons (make-phone 281 123 4567) '()))
(check-expect (replace LOP-EX2)
              (cons (make-phone 808 987 6543)
                    (cons (make-phone 281 321 0123)
                          (cons (make-phone 281 123 4567) '()))))

(define (replace lop)
  (cond [(empty? lop) '()]
        [else
         (if (area=713? (first lop))
             (cons (area->281 (first lop)) (replace (rest lop)))
             (cons (first lop) (replace (rest lop))))]))

;; Phone -> Boolean
;; checks if area code of a given Phone ph is 713

(check-expect (area=713? (make-phone 713 123 4567)) #true)
(check-expect (area=713? (make-phone 281 123 4567)) #false)

(define (area=713? ph)
  (= (phone-area ph) 713))

;; Phone -> Phone
;; replaces the area code of a given Phone ph with 281

(check-expect (area->281 (make-phone 713 123 4567))
              (make-phone 281 123 4567))

(define (area->281 ph)
  (make-phone 281
              (phone-switch ph)
              (phone-four ph)))

;;; APPLICATION --------------------------------------------

(test)
