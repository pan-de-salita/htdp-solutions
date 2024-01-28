#lang racket
(require test-engine/racket-tests)

;; List-of-strings -> List-of-strings
;; converts all occurances of "robot" to
;; "r2d2" in a List-of-strings los
(check-expect (subst-robot '()) '())
(check-expect (subst-robot (cons "robot" (cons "robot" '())))
              (cons "r2d2" (cons "r2d2" '())))
(check-expect (subst-robot (cons "thing" (cons "robot" '())))
              (cons "thing" (cons "r2d2" '())))

(define (subst-robot los)
  (cond [(empty? los) los]
        [else (if (string=? (first los) "robot")
                  (cons "r2d2" (subst-robot (rest los)))
                  (cons (first los) (subst-robot (rest los))))]))

;; String String List-of-strings -> List-of-strings
;; converts all occurances of old in given
;; List-of-strings los into new
(check-expect (substitute "A" "a" '()) '())
(check-expect (substitute "A" "a" (cons "a" (cons "B" (cons "C" '()))))
              (cons "A" (cons "B" (cons "C" '()))))
(check-expect (substitute "A" "a" (cons "a" (cons "B" (cons "C" (cons "a"'())))))
              (cons "A" (cons "B" (cons "C" (cons "A" '())))))

(define (substitute new old los)
  (cond [(empty? los) los]
        [else (if (string=? (first los) old)
                  (cons new (substitute new old (rest los)))
                  (cons (first los) (substitute new old (rest los))))]))

(test)
