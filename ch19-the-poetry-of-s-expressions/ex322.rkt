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

#|

BT-D0:

___[15]
____/
___/
__/
[87]

BT-D1:

[15]
__\
___\
____\
____[24]

|#

;; BT Number -> Boolean
;; checks if n occurs in a-bt

(define (contains-bt? bt n)
  (match bt
    [(? no-info?) #f]
    [(? node?)
     (or (= (node-ssn bt) n)
         (contains-bt? (node-left bt) n)
         (contains-bt? (node-right bt) n))]))

(check-expect (contains-bt? BT-H 87) #t)
(check-expect (contains-bt? BT-I 87) #f)
(check-expect (contains-bt? BT-D0 87) #t)
(check-expect (contains-bt? BT-D1 87) #f)

(test)
