#lang racket
(require test-engine/racket-tests)

;; Number -> [List-of Number]
;; tabulates sin between n and 0 (incl.) in a list.

(check-expect (tab-sin 0) (list (sin 0)))
(check-within (tab-sin 1) (list (sin 1) (sin 0)) 0.001)
(check-within (tab-sin 2) (list (sin 2) (sin 1) (sin 0)) 0.001)

(define (tab-sin n)
  (cond [(= n 0) (list (sin 0))]
        [else (cons (sin n) (tab-sin (sub1 n)))]))

;; Number -> [List-of Number]
;; tabulates sqrt between n and 0 (incl.) in a list.

(check-expect (tab-sqrt 0) (list (sqrt 0)))
(check-within (tab-sqrt 1) (list (sqrt 1) (sqrt 0)) 0.001)
(check-within (tab-sqrt 2) (list (sqrt 2) (sqrt 1) (sqrt 0)) 0.001)

(define (tab-sqrt n)
  (cond [(= n 0) (list (sqrt 0))]
        [else (cons (sqrt n) (tab-sqrt (sub1 n)))]))

;; Number [Number -> [List-of Number]] -> [List-of Number]
;; tabulates f between n and 0 (incl.) in a list.

(define (tabulate n f)
  (cond [(= n 0) (list (f n))]
        [else (cons (f n) (tabulate (sub1 n) f))]))

;; Number -> [List-of Number]
;; tabulates sin between n and 0 (incl.) in a list.

(check-expect (tab-sin-from-abstract 0) (list (sin 0)))
(check-within (tab-sin-from-abstract 1) (list (sin 1) (sin 0)) 0.001)
(check-within (tab-sin-from-abstract 2) (list (sin 2) (sin 1) (sin 0)) 0.001)

(define (tab-sin-from-abstract n)
  (tabulate n sin))

;; Number -> [List-of Number]
;; tabulates sqrt between n and 0 (incl.) in a list.

(check-expect (tab-sqrt-from-abstract 0) (list (sqrt 0)))
(check-within (tab-sqrt-from-abstract 1) (list (sqrt 1) (sqrt 0)) 0.001)
(check-within (tab-sqrt-from-abstract 2) (list (sqrt 2) (sqrt 1) (sqrt 0)) 0.001)

(define (tab-sqrt-from-abstract n)
  (tabulate n sqrt))

;; Number -> [List-of Number]
;; tabulates sqr between n and 0 (incl.) in a list.

(check-expect (tab-sqr-from-abstract 0) (list (sqr 0)))
(check-expect (tab-sqr-from-abstract 1) (list (sqr 1) (sqr 0)))
(check-expect (tab-sqr-from-abstract 2) (list (sqr 2) (sqr 1) (sqr 0)))

(define (tab-sqr-from-abstract n)
  (tabulate n sqr))

;; Number -> [List-of Number]
;; tabultes tan between n and 0 (incl.) in a list.

(check-expect (tab-tan-from-abstract 0) (list (tan 0)))
(check-within (tab-tan-from-abstract 1) (list (tan 1) (tan 0)) 0.001)
(check-within (tab-tan-from-abstract 2) (list (tan 2) (tan 1) (tan 0)) 0.001)

(define (tab-tan-from-abstract n)
  (tabulate n tan))

(test)
