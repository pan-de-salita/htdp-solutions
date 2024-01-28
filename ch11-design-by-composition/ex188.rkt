#lang racket
(require test-engine/racket-tests)

;;; data definitions

(define-struct email [from date message])
;; an Email a structure:
;;     (make-email String Number String)
;; i.e. (make-email f d m) represents text m
;; sent by f, d seconds after the beginning
;; of time
;; examples:
(define email0 (make-email "Murphy" 1 "meow"))
(define email1 (make-email "Taiga" 2 "meow"))
(define email2 (make-email "Konam" 3 "meow"))
(define email3 (make-email "Darth" 4 "meow"))

;; a List-of-emails is one of:
;; - '()
;; - (cons Email List-of-emails)
;; i.e. a list of Email instances
;; examples:
(define aloe0 '())
(define aloe1 (list email0))
(define aloe2 (list email0 email1 email2 email3))
(define aloe3 (list email2 email1 email0 email3))
(define aloe4 (list email3 email2 email1 email0))
(define aloe5 (list email3 email2 email0 email1))

;; an NE-list-of-emails is one of:
;; - (cons Email '())
;; - (cons Email NE-list-of-emails)
;; i.e. a non-empty List-of-emails

;;; functions

;; List-of-emails -> List-of-emails
;; sorts the emails in a List-of-emails aloe
;; in descending order according to date

(check-expect (sort>/email-date aloe0) aloe0)
(check-expect (sort>/email-date aloe1) aloe1)
(check-expect (sort>/email-date aloe2) aloe4)
(check-expect (sort>/email-date aloe3) aloe4)
(check-expect (sort>/email-date aloe5) aloe4)

(define (sort>/email-date aloe)
  (cond [(empty? aloe) '()]
        [else (insert/email-date (car aloe) (sort>/email-date (cdr aloe)))]))

;; Email List-of-emails -> List-of-emails
;; inserts given Email e into a List-of-emails
;; aloe that's sorted by date

(check-expect (insert/email-date email1 '()) (list email1))
(check-expect (insert/email-date email1 (list email2)) (list email2 email1))
(check-expect (insert/email-date email1 (list email0)) (list email1 email0))
(check-expect (insert/email-date email1 (list email3 email2 email0)) aloe4)

(define (insert/email-date e aloe)
  (cond [(empty? aloe) (cons e '())]
        [else (if (>= (email-date e) (email-date (car aloe)))
                  (cons e aloe)
                  (cons (car aloe) (insert/email-date e (cdr aloe))))]))

;; List-of-emails -> List-of-emails
;; sorts a List-of-emails aloe alphabetically by name

(check-expect (sort>/email-name aloe0) aloe0)
(check-expect (sort>/email-name aloe1) aloe1)
(check-expect (sort>/email-name aloe2) aloe5)
(check-expect (sort>/email-name aloe3) aloe5)
(check-expect (sort>/email-name aloe4) aloe5)
(check-expect (sort>/email-name aloe5) aloe5)

(define (sort>/email-name aloe)
  (cond [(empty? aloe) '()]
        [else (insert/email-name (car aloe) (sort>/email-name (cdr aloe)))]))

;; Email List-of-emails -> List-of-emails
;; inserts given Email e into a List-of-emails
;; aloe sorted alphabetically

(check-expect
 (insert/email-name
  (make-email "b" 9 "hi") '())
 (cons (make-email "b" 9 "hi") '()))
(check-expect
 (insert/email-name
  (make-email "b" 9 "hi")
  (list (make-email "a" 9 "hi")))
 (list (make-email "a" 9 "hi")
       (make-email "b" 9 "hi")))
(check-expect
 (insert/email-name
  (make-email "b" 9 "hi")
  (list (make-email "c" 9 "hi")))
 (list (make-email "b" 9 "hi")
       (make-email "c" 9 "hi")))
(check-expect
 (insert/email-name
  (make-email "b" 9 "hi")
  (list (make-email "a" 9 "hi")
        (make-email "c" 9 "hi")
        (make-email "d" 9 "hi")))
 (list (make-email "a" 9 "hi")
       (make-email "b" 9 "hi")
       (make-email "c" 9 "hi")
       (make-email "d" 9 "hi")))

(define (insert/email-name e aloe)
  (cond [(empty? aloe) (cons e '())]
        [else (if (string<? (email-from e) (email-from (car aloe)))
                  (cons e aloe)
                  (cons (car aloe) (insert/email-name e (cdr aloe))))]))

;;; application

(test)
