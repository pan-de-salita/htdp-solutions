#lang racket
(require test-engine/racket-tests)

;; a List-of-Names is one of:
;; - '()
;; - (cons String List-of-Names)
;; i.e. a list of invitees, by last name
(define a-list-of-names-example-0
  (cons "last name 0"
        (cons "last name 1"
              (cons "last name 2"
                    (cons "last name 3"
                          (cons "last name 4" '()))))))
(define a-list-of-names-example-1
  (cons "last name 0"
        (cons "last name 1"
              (cons "Flatt"
                    (cons "last name 3"
                          (cons "last name 4" '()))))))

;; List-of-Names -> Boolean
;; determines whether "Flatt" is on a list-of-names
(check-expect (contains-flatt? '()) #false)
(check-expect (contains-flatt? (cons "Find" '())) #false)
(check-expect (contains-flatt? (cons "Flatt" '())) #true)
(check-expect (contains-flatt?
               (cons "A" (cons "Flatt" (cons "C" '()))))
              #true)
(check-expect (contains-flatt?
               (cons "A" (cons "B" (cons "C" '()))))
              #false)

(define (contains-flatt? a-list-of-names)
  (cond
    [(empty? a-list-of-names) #false]
    [(cons? a-list-of-names)
     (or (string=? (first a-list-of-names) "Flatt")
         (contains-flatt? (rest a-list-of-names)))]))

(test)
