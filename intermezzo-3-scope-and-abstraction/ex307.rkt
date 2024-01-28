#lang racket
(require test-engine/racket-tests)

;; String [List-of string] -> String
;; retrieves the first name on l-name that is
;; equal to, or an extension of, ref-name

(define (find-string ref-name l-name)
  (local (;; String -> [List-of String]
          (define (break-down str)
            (if (<= (string-length str) (string-length ref-name))
                (list str)
                (build-list
                 (add1 (- (string-length str)
                          (string-length ref-name)))
                 (lambda (x)
                   (substring str x (+ x (string-length ref-name))))))))
    (for/or ([a-name l-name])
      (if (not (false? (member ref-name (break-down a-name))))
          a-name
          #f))))

;; is equal to:

(define (find-string.v2 ref-name l-name)
  (for/or ([a-name l-name])
    (if (string-contains? a-name ref-name) a-name #f)))

;; test:

(check-expect (find-string "lin" '())
              (find-string.v2 "lin" '()))
(check-expect (find-string "lin" '("cat" "feline" "felix"))
              (find-string.v2 "lin" '("cat" "feline" "felix")))

;; String [List-of String] -> Boolean
;; checks if no name on l-name is longer than ref-name

(check-expect (none-exceeds? "cat" '()) #t)
(check-expect (none-exceeds? "cat" '("a" "b" "c")) #t)
(check-expect (none-exceeds? "cat" '("abc" "def" "ghi")) #t)
(check-expect (none-exceeds? "cat" '("cat" "dog" "bird")) #f)

(define (none-exceeds? ref-name l-name)
  (for/and ([a-name l-name])
    (<= (string-length a-name) (string-length ref-name))))

;; String [List-of String] -> Boolean
;; like, none-exceeds, checks if no name on l-name is longer than ref-name

(check-expect (none-exceeds?.v2 "cat" '()) #t)
(check-expect (none-exceeds?.v2 "cat" '("a" "b" "c")) #t)
(check-expect (none-exceeds?.v2 "cat" '("abc" "def" "ghi")) #t)
(check-expect (none-exceeds?.v2 "cat" '("cat" "dog" "bird")) #f)

(define (none-exceeds?.v2 ref-name l-name)
  (not (for/or ([a-name l-name])
         (> (string-length a-name) (string-length ref-name)))))

(test)
