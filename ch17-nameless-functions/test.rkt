#lang racket
(require test-engine/racket-tests)

;;;; identity-matrix practice -----------------------------------------------

;; a Row is a list containing:
;; - zero or more 0s
;; - exactly one 1

;; an IdentityMatrix is a [List-of Row]

(define matrix-of-1 '((1)))
(define matrix-of-2
  '((1 0)
    (0 1)))
(define matrix-of-3
  '((1 0 0)
    (0 1 0)
    (0 0 1)))
(define matrix-of-4
  '((1 0 0 0)
    (0 1 0 0)
    (0 0 1 0)
    (0 0 0 1)))

;; Number -> IdentityMatrix
;; using build-list, returns an IdentityMatrix of size n

(check-expect (identity-matrix 1) matrix-of-1)
(check-expect (identity-matrix 2) matrix-of-2)
(check-expect (identity-matrix 3) matrix-of-3)
(check-expect (identity-matrix 4) matrix-of-4)

(define (identity-matrix n)
  (build-list n
              ;; Number -> Row
              ;; builds a Row of length n
              (lambda (row-number)
                (build-list n
                            ;; Number -> Number
                            ;; determines whether a digit should be 0 or 1
                            (lambda (digit-index)
                              (if (= row-number digit-index) 1 0))))))

;; Number -> IdentityMatrix
;; like identity-matrix, using build-list, returns an IdentityMatrix of size n

(check-expect (identity-matrix.v2 1) (identity-matrix 1))
(check-expect (identity-matrix.v2 2) (identity-matrix 2))
(check-expect (identity-matrix.v2 3) (identity-matrix 3))
(check-expect (identity-matrix.v2 4) (identity-matrix 4))

(define (identity-matrix.v2 n)
  (local (;; Number -> Row
          ;; builds a Row of length n
          (define (build-row row-number)
            (local (;; Number -> Number
                    ;; determines whether a digit should be 0 or 1
                    (define (zero-or-one digit-index)
                      (if (= row-number digit-index) 1 0)))
              (build-list n zero-or-one))))
    (build-list n build-row)))

;; Number -> IdentityMatrix
;; like identity-matrix, using build-list, returns an IdentityMatrix of size n

(check-expect (identity-matrix.v3 1) (identity-matrix 1))
(check-expect (identity-matrix.v3 2) (identity-matrix 2))
(check-expect (identity-matrix.v3 3) (identity-matrix 3))
(check-expect (identity-matrix.v3 4) (identity-matrix 4))

(define (identity-matrix.v3 n)
  (cond [(zero? n) '()]
        [else (cons (build-list n (lambda (x) (if (= x 0) 1 0)))
                    (map (lambda (row) (cons 0 row)) (identity-matrix.v3 (sub1 n))))]))

;; Number -> IdentityMatrix
;; using foldr, returns an IdentityMatrix of size n

(check-expect (identity-matrix.v4 1) (identity-matrix 1))
(check-expect (identity-matrix.v4 2) (identity-matrix 2))
(check-expect (identity-matrix.v4 3) (identity-matrix 3))
(check-expect (identity-matrix.v4 4) (identity-matrix 4))

(define (identity-matrix.v4 n)
  (foldr (lambda (index-of-one occurring-list)
           (cons (build-list n
                             (lambda (index-of-digit)
                               (if (= index-of-digit index-of-one) 1 0)))
                 occurring-list))
         '()
         (build-list n (lambda (row-number) row-number))))

;;;; find-name practice -----------------------------------------------------

;; String [List-of String] -> Boolean
;; determines whether any of the names on l-name is equal to or an extension of reference-name

(check-expect (name-found? "ABC" '()) #f)
(check-expect (name-found? "ABC" '("abc" "123" "xyz")) #f)
(check-expect (name-found? "ABC" '("ABC" "123" "xyz")) #t)
(check-expect (name-found? "ABC" '("123" "ABC" "xyz")) #t)
(check-expect (name-found? "ABC" '("123" "xyz" "ABC")) #t)
(check-expect (name-found? "ABC" '("123" "xyzABCxyz" "abc")) #t)

(define (name-found? reference-name l-name)
  (ormap (lambda (name-from-list)
           (not (boolean?
                 (member reference-name
                         (build-list (add1 (- (string-length name-from-list) (string-length reference-name)))
                                     (lambda (starting-index)
                                       (substring name-from-list starting-index (+ starting-index (string-length reference-name)))))))))
         l-name))

;; String [List-of String] -> Boolean
;; like name-found, determines whether any of the names on l-name is equal to or an extension of reference-name

(check-expect (name-found? "ABC" '()) (name-found?.v2 "ABC" '()))
(check-expect (name-found? "ABC" '("abc" "123" "xyz")) (name-found?.v2 "ABC" '("abc" "123" "xyz")))
(check-expect (name-found? "ABC" '("ABC" "123" "xyz")) (name-found?.v2 "ABC" '("ABC" "123" "xyz")))
(check-expect (name-found? "ABC" '("123" "ABC" "xyz")) (name-found?.v2 "ABC" '("123" "ABC" "xyz")))
(check-expect (name-found? "ABC" '("123" "xyz" "ABC")) (name-found?.v2 "ABC" '("123" "xyz" "ABC")))
(check-expect (name-found? "ABC" '("123" "xyzABCxyz" "abc")) (name-found?.v2 "ABC" '("123" "xyzABCxyz" "abc")))

(define (name-found?.v2 reference-name l-name)
  (local (;; Name Name -> Boolean
          ;; checks whether name-from-list is equal to or an extension of reference-name
          (define (equal-to-or-extension? name-from-list)
            (cond [(< (string-length name-from-list) (string-length reference-name)) #f]
                  [else (or (string=? (substring name-from-list 0 (string-length reference-name)) reference-name)
                            (equal-to-or-extension? (substring name-from-list 1)))])))
    (ormap equal-to-or-extension? l-name)))

;;;; apply-to-all  ----------------------------------------------------------

;; [X -> Y] Number -> Y
;; applies F n times to l-x

(check-expect ((apply-n-times cdr 0) '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect ((apply-n-times cdr 1) '(1 2 3 4 5)) '(2 3 4 5))
(check-expect ((apply-n-times cdr 2) '(1 2 3 4 5)) '(3 4 5))
(check-expect ((apply-n-times cdr 3) '(1 2 3 4 5)) '(4 5))

(define (apply-n-times F n)
  (lambda (l-x)
    (if (zero? n)
        l-x
        (F [(apply-n-times F (sub1 n)) l-x]))))

;;;; application

(test)
