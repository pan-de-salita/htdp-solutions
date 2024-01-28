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
(define BT-D0 (make-node 15 'd NONE BT-I))
(define BT-D1 (make-node 15 'd BT-H NONE))
