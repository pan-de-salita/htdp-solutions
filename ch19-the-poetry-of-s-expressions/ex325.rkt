#lang racket
(require test-engine/racket-tests)

(define-struct no-info [] #:transparent)
(define NONE (make-no-info))

(define-struct node [ssn name left right] #:transparent)
;; a BT (short for BinaryTree) is one of:
;; - NONE
;; - (make-node Number Symbol BT BT)
;; examples
(define BT-H (make-node 87 'h NONE NONE))
(define BT-I (make-node 24 'i NONE NONE))
(define BT-D0 (make-node 15 'd BT-H NONE))
(define BT-D1 (make-node 15 'd NONE BT-I))

;; a BST (short for BinarySearchTree) is one of:
;; - NONE
;; - (make-node ssn0(Number) Symbol L(BT) R(BT)) if
;;   + L is a BST,
;;   + R is a BST,
;;   + all ssn fields in L are smaller than ssn0,
;;   + all ssn fields in R are larger than ssn0.
(define BT-10 (make-node 10 'ten NONE NONE))
(define BT-24 (make-node 24 'twenty-four NONE NONE))
(define BT-15 (make-node 15 'fifteen BT-10 BT-24))
(define BT-29 (make-node 29 'twenty-nine BT-15 NONE))
(define BT-77 (make-node 77 'seventy-seven NONE NONE))
(define BT-99 (make-node 99 'ninety-nine NONE NONE))
(define BT-95 (make-node 95 'ninety-five NONE BT-99))
(define BT-89 (make-node 89 'eighty-nine BT-77 BT-95))

(define BT-63 (make-node 63 'sixty-three BT-29 BT-89))
;; is a BST since when drawn as a figure and read from left
;; to right, gives us:
;; 10 15 24 29 63 77 89 95 99
;; which are oredered in ascending order

;; BST Number -> [Maybe Symbol]
;; if the tree contains a node whose ssn field is n,
;; produces the value of the name field in that node,
;; else returns NONE

(check-expect (search-bst BT-63 63) 'sixty-three)
(check-expect (search-bst BT-63 10) 'ten)
(check-expect (search-bst BT-63 77) 'seventy-seven)
(check-expect (search-bst BT-63 15) 'fifteen)
(check-expect (search-bst BT-63 95) 'ninety-five)
(check-expect (search-bst BT-63 0) NONE)

(define (search-bst bst n)
  (match bst
    [(? no-info?) NONE]
    [(node ssn name left right)
     (cond [(= n ssn) name]
           [(< n ssn) (search-bst left n)]
           [(> n ssn) (search-bst right n)])]))

;; [List-of Number] -> Boolean
;; rewritten version of search of ex189,
;; which determines whether a number occurs in a
;; list of numbers sorted in ascending order

(define (search-sorted sorted-l n)
  (cond
    [(null? sorted-l) #f]
    [(> (car sorted-l) n) #f]
    [else (or (= (car sorted-l) n)
              (search-sorted (cdr sorted-l) n))]))

(check-expect (search-sorted '() 3) #f)
(check-expect (search-sorted '(1 2) 3) #f)
(check-expect (search-sorted '(1 2 3) 3) #t)
(check-expect (search-sorted '(1 2 3 4) 3) #t)

;; [List-of Number] -> Boolean
;; re-rewritten version of search of ex189,
;; which determines whether a number occurs in a
;; list of numbers sorted in ascending order

(define (search-sorted.v2 sorted-l n)
  (cond
    [(null? sorted-l) #f]
    [else
     (local ((define l-sorted-halved
               (local ((define median
                         (/ (+ (first sorted-l) (last sorted-l)) 2))
                       ;; [X X -> Boolean] -> [List-of Number]
                       (define (halve-l-sorted cmp)
                         (filter (lambda (from-l) (cmp from-l median)) sorted-l)))
                 (cond
                   [(< n median) (halve-l-sorted <)]
                   [(> n median) (halve-l-sorted >)]))))
       (or (= (car sorted-l) n)
           (search-sorted.v2 l-sorted-halved n)))]))

(check-expect (search-sorted.v2 '() 3) (search-sorted '() 3))
(check-expect (search-sorted.v2 '(1 2) 3) (search-sorted '(1 2) 3))
(check-expect (search-sorted.v2 '(1 2 3) 3) (search-sorted '(1 2 3) 3))
(check-expect (search-sorted.v2 '(1 2 3 4) 3) (search-sorted '(1 2 3 4) 3))

(test)
