#lang racket
(require racket/trace)
(require test-engine/racket-tests)
(require 2htdp/batch-io)

;; Main function
(define (main in-fst in-lst in-signature out)
  (write-file out
              (letter (read-file in-fst)
                      (read-file in-lst)
                      (read-file in-signature))))

;; Letter writing function
(define (letter fst lst signature-name)
  (string-append (opening fst)
                 "\n\n"
                 (body fst lst)
                 "\n\n"
                 (closing signature-name)))

;; Letter opening
(define (opening fst)
  (string-append "Dear " fst ","))

;; Letter body
(define (body fst lst)
  (string-append "We have discovered that all people with the" "\n"
                 "last name " lst " have won our lottery. So, " "\n"
                 fst ", " "hurry and pick up your prize."))

;; Letter closing
(define (closing signature-name)
  (string-append "Sincerely,"
                 "\n\n"
                 signature-name
                 "\n"))
