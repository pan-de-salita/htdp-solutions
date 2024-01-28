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

;; BT Number -> [Maybe Symbol]
;; produces the value of the name field in
;; a node structure whose ssn field is n,
;; else produces #f

(define (search-bt bt n)
  (match bt
    [(? no-info?) #f]
    [(node ssn name left right)
     (cond [(= ssn n) name]
           [(contains-bt? left n) (search-bt left n)]
           [else (search-bt right n)])]))

(check-expect (search-bt BT-H 87) 'h)
(check-expect (search-bt BT-I 87) #f)
(check-expect (search-bt BT-D0 87) 'h)
(check-expect (search-bt BT-D1 87) #f)
(check-expect (search-bt BT-H 24) #f)
(check-expect (search-bt BT-I 24) 'i)
(check-expect (search-bt BT-D0 24) #f)
(check-expect (search-bt BT-D1 24) 'i)

;; BT Number -> [Maybe Symbol]
;; produces the value of the name field in
;; a node structure whose ssn field is n,
;; else produces #f
;; NOTE: for instances where an ssn checks out,
;; this version seems faster albeit less readable?

(define (search-bt.v2 bt n)
  (match bt
    [(? no-info?) #f]
    [(node ssn name left right)
     (if (= ssn n)
         name
         (or (search-bt.v2 left n)
             (search-bt.v2 right n)))]))

(check-expect (search-bt.v2 BT-H 87) 'h)
(check-expect (search-bt.v2 BT-I 87) #f)
(check-expect (search-bt.v2 BT-D0 87) 'h)
(check-expect (search-bt.v2 BT-D1 87) #f)
(check-expect (search-bt.v2 BT-H 24) #f)
(check-expect (search-bt.v2 BT-I 24) 'i)
(check-expect (search-bt.v2 BT-D0 24) #f)
(check-expect (search-bt.v2 BT-D1 24) 'i)

(test)
