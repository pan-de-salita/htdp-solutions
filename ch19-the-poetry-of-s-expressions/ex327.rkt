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

;; BST Number Symbol -> BST
;; produces a BST just like bst, but in place of one NONE
;; creates a subtree containing the node structure:
;;   (mae-node Number Symbol NONE NONE)

(define (create-bst bst n s)
  (match bst
    [(? no-info?) (make-node n s NONE NONE)]
    [(node ssn name left right)
     (cond
       [(< n ssn) (make-node ssn name (create-bst left n s) right)]
       [(> n ssn) (make-node ssn name left (create-bst right n s))]
       [else bst])]))

;; [List-of [List-of Number Symbol]] -> BST
;; creates a BST according to given [List-of [List-of Number Symbol]]

(define (create-bst-from-list ll-num-sym)
  (match ll-num-sym
    [(? null?) NONE]
    [(cons (list num sym) rst)
     (create-bst (create-bst-from-list rst) num sym)]))

;; [List-of [List-of Numbe Symbol]] -> BST
;; like create-bst-from-list, creates a BST according to
;; given [List-of [List-of Number Symbol]]

(define (create-bst-from-list.v2 ll-num-sym)
  (foldr
   ;; [List-of Number Symbol] BST -> BST
   (lambda (node-details bst)
     (match node-details
       [(list num sym) (create-bst bst num sym)]))
   NONE
   ll-num-sym))

;; If you use an existing abstraction, you may still get this
;; tree but you may also get an “inverted” one. Why?
;;
;; using foldr yields a BST in the correct order because it
;; builds the tree starting with the rightmost element of the
;; given [List-of [List-of Number symbol]]. using foldl, on the
;; other hand, gives us the same BST with its order inverted
;; because it builds the tree starting with the leftmost element
;; of the given input.

;; tests

(check-expect (create-bst NONE 15 'd) (make-node 15 'd NONE NONE))
(check-expect (create-bst BT-D1 15 'd) BT-D1)
(check-expect
 (create-bst BT-D1 3 'three)
 (make-node 15 'd
            (make-node 3 'three NONE NONE)
            BT-I))
(check-expect
 (create-bst BT-D1 30 'thirty)
 (make-node 15 'd NONE
            (make-node 24 'i NONE
                       (make-node 30 'thirty NONE NONE))))
(check-expect
 (create-bst BT-D1 20 'twenty)
 (make-node 15 'd NONE
            (make-node 24 'i
                       (make-node 20 'twenty NONE NONE)
                       NONE)))
(check-expect (create-bst BT-63 63 'sixty-three) BT-63)
(check-expect (create-bst BT-63 24 'twenty-four) BT-63)
(check-expect
 (create-bst BT-63 2 'two)
 (node 63 'sixty-three
       (node 29 'twenty-nine
             (node 15 'fifteen
                   (node 10 'ten
                         (node 2 'two
                               (no-info)
                               (no-info))
                         (no-info))
                   (node 24 'twenty-four
                         (no-info)
                         (no-info)))
             (no-info))
       (node 89 'eighty-nine
             (node 77 'seventy-seven
                   (no-info)
                   (no-info))
             (node 95 'ninety-five
                   (no-info)
                   (node 99 'ninety-nine
                         (no-info)
                         (no-info))))))
(check-expect
 (create-bst BT-63 100 'one-hundred)
 (node 63 'sixty-three
       (node 29 'twenty-nine
             (node 15 'fifteen
                   (node 10 'ten
                         (no-info)
                         (no-info))
                   (node 24 'twenty-four
                         (no-info)
                         (no-info)))
             (no-info))
       (node 89 'eighty-nine
             (node 77 'seventy-seven
                   (no-info)
                   (no-info))
             (node 95 'ninety-five
                   (no-info)
                   (node 99 'ninety-nine
                         (no-info)
                         (node 100 'one-hundred
                               (no-info)
                               (no-info)))))))
(check-expect
 (create-bst BT-63 97 'ninety-seven)
 (node 63 'sixty-three
       (node 29 'twenty-nine
             (node 15 'fifteen
                   (node 10 'ten
                         (no-info)
                         (no-info))
                   (node 24 'twenty-four
                         (no-info)
                         (no-info)))
             (no-info))
       (node 89 'eighty-nine
             (node 77 'seventy-seven
                   (no-info)
                   (no-info))
             (node 95 'ninety-five
                   (no-info)
                   (node 99 'ninety-nine
                         (node 97 'ninety-seven
                               (no-info)
                               (no-info))
                         (no-info))))))
(check-expect
 (create-bst BT-63 18 'eighteen)
 (node 63 'sixty-three
       (node 29 'twenty-nine
             (node 15 'fifteen
                   (node 10 'ten
                         (no-info)
                         (no-info))
                   (node 24 'twenty-four
                         (node 18 'eighteen
                               (no-info)
                               (no-info))
                         (no-info)))
             (no-info))
       (node 89 'eighty-nine
             (node 77 'seventy-seven
                   (no-info)
                   (no-info))
             (node 95 'ninety-five
                   (no-info)
                   (node 99 'ninety-nine
                         (no-info)
                         (no-info))))))

(check-expect
 (create-bst-from-list
  '())
 NONE)
(check-expect
 (create-bst-from-list
  '((99 o)))
 (make-node 99 'o NONE NONE))
(check-expect
 (create-bst-from-list
  '((99 o)
    (77 l)
    (24 i)))
 (make-node 24 'i
            NONE
            (make-node 77 'l NONE
                       (make-node 99 'o NONE NONE))))
(check-expect
 (create-bst-from-list
  '((99 o)
    (77 l)
    (24 i)
    (10 h)))
 (make-node 10 'h
            NONE
            (make-node 24 'i
                       NONE
                       (make-node 77 'l
                                  NONE
                                  (make-node 99 'o NONE NONE)))))
(check-expect
 (create-bst-from-list
  '((99 ninety-nine)
    (77 seventy-seven)
    (24 twenty-four)
    (10 ten)
    (95 ninety-five)
    (15 fifteen)
    (89 eighty-nine)
    (29 twenty-nine)
    (63 sixty-three)))
 BT-63)

(check-expect
 (create-bst-from-list.v2
  '())
 NONE)
(check-expect
 (create-bst-from-list.v2
  '((99 o)))
 (make-node 99 'o NONE NONE))
(check-expect
 (create-bst-from-list.v2
  '((99 o)
    (77 l)
    (24 i)))
 (make-node 24 'i
            NONE
            (make-node 77 'l NONE
                       (make-node 99 'o NONE NONE))))
(check-expect
 (create-bst-from-list.v2
  '((99 o)
    (77 l)
    (24 i)
    (10 h)))
 (make-node 10 'h
            NONE
            (make-node 24 'i
                       NONE
                       (make-node 77 'l
                                  NONE
                                  (make-node 99 'o NONE NONE)))))
(check-expect
 (create-bst-from-list.v2
  '((99 ninety-nine)
    (77 seventy-seven)
    (24 twenty-four)
    (10 ten)
    (95 ninety-five)
    (15 fifteen)
    (89 eighty-nine)
    (29 twenty-nine)
    (63 sixty-three)))
 BT-63)
(check-expect
 (create-bst-from-list.v2
  '((1 one)
    (2 two)
    (3 three)
    (4 four)
    (5 five)))
 (make-node 5 'five
            (make-node 4 'four
                       (make-node 3 'three
                                  (make-node 2 'two
                                             (make-node 1 'one NONE NONE)
                                             NONE)
                                  NONE)
                       NONE)
            NONE))
(check-expect
 (create-bst-from-list.v2
  '((1 one)
    (2 two)
    (3 three)
    (4 four)
    (5 five)))
 (create-bst-from-list
  '((1 one)
    (2 two)
    (3 three)
    (4 four)
    (5 five))))

(test)
