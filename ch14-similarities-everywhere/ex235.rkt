#lang racket
(require test-engine/racket-tests)

;; List-of-Strings String -> Boolean
;; determines whether a given List-of-Strings contains a given String.

(check-expect (contains? '() "cat") #f)
(check-expect (contains? '("cat" "dog" "bear") "cat") #t)
(check-expect (contains? '("cat" "dog" "bear") "tanuki") #f)

(define (contains? a-list-of-strings a-string)
  (cond [(empty? a-list-of-strings) #f]
        [else (or (string=? a-string (car a-list-of-strings))
                  (contains? (cdr a-list-of-strings) a-string))]))

;; List-of-Strings -> Boolean
;; determines whether a given List-of-Strings contains the String "atom".

(check-expect (contains-atom? '()) #f)
(check-expect (contains-atom? '("cat" "dog" "atom")) #t)
(check-expect (contains-atom? '("cat" "dog" "bear")) #f)

(define (contains-atom? a-list-of-strings)
  (contains? a-list-of-strings "atom"))

;; List-of-Strings -> Boolean
;; determines whether a given List-of-Strings contains the String "basic".

(check-expect (contains-basic? '()) #f)
(check-expect (contains-basic? '("cat" "dog" "basic")) #t)
(check-expect (contains-basic? '("cat" "dog" "bear")) #f)

(define (contains-basic? a-list-of-strings)
  (contains? a-list-of-strings "basic"))

;; List-of-Strings -> Boolean
;; determines whether a given List-of-Strings contains the String "zoo".

(check-expect (contains-zoo? '()) #f)
(check-expect (contains-zoo? '("cat" "dog" "zoo")) #t)
(check-expect (contains-zoo? '("cat" "dog" "bear")) #f)

(define (contains-zoo? a-list-of-strings)
  (contains? a-list-of-strings "zoo"))

(test)
