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
(define a-list-of-names-example-2
  (cons "Fagan"
        (cons "Findler"
              (cons "Fisler"
                    (cons "Flanagan"
                          (cons "Flatt"
                                (cons "Felleissen"
                                      (cons "Friedman" '()))))))))

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
    [else (cond
            [(string=? (first a-list-of-names) "Flatt") #true]
            [else (contains-flatt? (rest a-list-of-names))])]))

;; this version of contain-flatt? produces the same
;; results as the old version because it uses the same
;; logic: check if (first a-list-of-names) contains
;; "Flatt", else check if (rest a-list-of-names) contains
;; the String by applying contain-flatt?. if both clauses
;; yield #false, the function ultimately returns #false.
;;
;; the new version is clearer because it explicitly says
;; to check (first a-list-of-names) before recursing over
;; (rest a-list-of=names) with contains-flatt?. in the old
;; version, this procedure is abstracted away with the keyword
;; or.

(test)
