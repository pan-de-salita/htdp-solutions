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

(define BT/87 (make-node 87 'eighty-seven NONE NONE))
(define BT/24 (make-node 24 'twenty-four NONE NONE))
(define BT/15 (make-node 15 'fifteen BT/87 BT/24))
(define BT/29 (make-node 29 'twenty-nine BT/15 NONE))
(define BT/33 (make-node 33 'thirty-three NONE NONE))
(define BT/99 (make-node 99 'ninety-nine NONE NONE))
(define BT/95 (make-node 95 'ninety-five NONE BT/99))
(define BT/89 (make-node 89 'eighty-nine BT/33 BT/95))

(define BT/63 (make-node 63 'sixty-three BT/29 BT/89))
;; not a BST since, applying the samebreakdown as for
;; BST-63, gives us:
;; 87 15 24 29 63 33 89 95 99
;; which are NOT ordered in ascending order

;; BT -> [List-of Number]
;; produces the sequence of all ssn Numbers in a BT
;; from Left to Right

(define (in-order bt)
  (match bt
    [(? no-info?) '()]
    [(node ssn name left right)
     (append (in-order left) (list ssn) (in-order right))]))

(check-expect (in-order BT-D0) '(87 15))
(check-expect (in-order BT-D1) '(15 24))
(check-expect (in-order BT-63) '(10 15 24 29 63 77 89 95 99))
(check-expect (in-order BT/63) '(87 15 24 29 63 33 89 95 99))

;; BT -> Boolean
;; checks if a BT is a BST

(define (bst? bt)
  (local (;; [List-of Number]
          ;; [Number Number -> Boolean]
          ;; -> Boolean
          (define (sorted-bt? l cmp)
            (match l
              [(cons fst '()) #t]
              [(cons fst rst)
               (and (cmp fst (car rst))
                    (sorted-bt? rst cmp))])))
    (sorted-bt? (in-order bt) <)))

(check-expect (bst? BT-D0) #f)
(check-expect (bst? BT-D1) #t)
(check-expect (bst? BT-63) #t)
(check-expect (bst? BT/63) #f)

(test)
