;; Exercise 26. What do you expect as the value of this program:
;;
;; (define (string-insert s i)
;;   (string-append (substring s 0 i)
;;                  "_"
;;                  (substring s i)))
;;
;; (string-insert "helloworld" 6)

#lang racket
(require racket/trace)
(require test-engine/racket-tests)

(define (string-insert s i)
  (string-append (substring s 0 i)
                 "_"
                 (substring s i)))

(check-expect (string-insert "helloworld" 6) "hellow_orld")
(test)
