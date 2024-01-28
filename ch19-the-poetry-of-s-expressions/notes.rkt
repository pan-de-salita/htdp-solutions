#lang racket
(require test-engine/racket-tests)

;; TREES -------------------------------------------------------------------------------

;; NOTE: to avoid pointless data definitions, a self-referential
;; data definition should have several clauses with at least one
;; of them no referring back to the data definition

;; 2 insights:
;; - we are not looking for a data definition that describe how to
;; generate instances of child structures but for a data definition
;; that describes how to represent family trees.
;; - the data definition consists of two clauses, one for the variant
;; describing unknown family trees and another one for known family trees:

(define-struct child [father mother name year eyes] #:transparent)
;; a Child is a structure:
;;   (make-child Child Child String Number String)
(define-struct no-parent [] #:transparent)
(define NP (make-no-parent))
;; an FT (short for family tree) is one of:
;; - NP
;; - (make-child FT FT String Number String)

;; oldest generation:
(define CARL (make-child NP NP "Carl" 1926 "green"))
(define BETTINA (make-child NP NP "Bettina" 1926 "green"))

;; middle generation:
(define ADAM (make-child CARL BETTINA "Adam" 1950 "hazel"))
(define DAVE (make-child CARL BETTINA "Dave" 1955 "black"))
(define EVA (make-child CARL BETTINA "Eva" 1965 "blue"))
(define FRED (make-child NP NP "Fred" 1966 "pink"))

;; youngest generation
(define GUSTAV (make-child FRED EVA "Gustav" 1988 "brown"))

;; FT -> ???
;; ...

#|
(define (fun-FT-draft an-ftree)
  (cond [(no-parent? an-ftree) ...]
        [else (... (child-father an-ftree) ...
               ... (child-mother an-ftree) ...
               ... (child-name an-ftree) ...
               ... (child-year an-ftree) ...
               ... (child-eyes an-ftree) ...)]))
|#

;; NOTE: if a data definition refers to itself, the function is
;; likely to recur and templates indicate so with suggestive natural
;; recursions. the definition FT has two self-references (child-father
;; and child-mother), and the template therefore needs two such recurions:

#|
(define (fun-FT an-ftree)
  (cond [(no-parent? an-ftree) ...]
        [else (... (fun-FT (child-father an-ftree)) ...
               ... (fun-FT (child-mother an-ftree)) ...
               ... (child-name an-ftree) ...
               ... (child-year an-ftree) ...
               ... (child-eyes an-ftree) ...)]))
|#

;; FT -> Boolean
;; checks if an-ftree contains a child structure with
;; "blue" in the eyes field

(check-expect (blue-eyed-child? CARL) #f)
(check-expect (blue-eyed-child? BETTINA) #f)
(check-expect (blue-eyed-child? ADAM) #f)
(check-expect (blue-eyed-child? DAVE) #f)
(check-expect (blue-eyed-child? EVA) #t)
(check-expect (blue-eyed-child? FRED) #f)
(check-expect (blue-eyed-child? GUSTAV) #t) ;; Eva has blue eyes

(define (blue-eyed-child? an-ftree)
  (cond
    [(no-parent? an-ftree) #f]
    [else (or (string=? (child-eyes an-ftree) "blue")
              (blue-eyed-child? (child-father an-ftree))
              (blue-eyed-child? (child-mother an-ftree)))]))

;; FORESTS -----------------------------------------------------------------------------

;; an FF (short for family forest) is one of:
;; - '()
;; - (cons FT FF)
;; a family forest represents several familits (say, a town)
;; and their ancestor trees

(define FF1 (list CARL BETTINA))
(define FF2 (list FRED EVA))
(define FF3 (list FRED EVA CARL))

;; FF -> Boolean
;; checks if an FF contains a Child with "blue" in the eyes field

(check-expect (blue-eyed-child-in-forest? FF1) #f)
(check-expect (blue-eyed-child-in-forest? FF2) #t)
(check-expect (blue-eyed-child-in-forest? FF3) #t)

(define (blue-eyed-child-in-forest? ff)
  (cond [(empty? ff) #f]
        [else (or (blue-eyed-child? (car ff))
                  (blue-eyed-child-in-forest? (cdr ff)))]))

;; FF -> Boolean
;; like blue-eyed-child-in-forest?, checks if an FF contains a Child
;; with "blue" in the eyes field

(check-expect (blue-eyed-child-in-forest?.v2 FF1)
              (blue-eyed-child-in-forest? FF1))
(check-expect (blue-eyed-child-in-forest?.v2 FF2)
              (blue-eyed-child-in-forest? FF2))
(check-expect (blue-eyed-child-in-forest?.v2 FF3)
              (blue-eyed-child-in-forest? FF3))

(define (blue-eyed-child-in-forest?.v2 ff)
  (for/or ([ft ff])
    (blue-eyed-child? ft)))

;; FF -> Boolean
;; like blue-eyed-child-in-forest?, checks if an FF contains a Child
;; with "blue" in the eyes field

(check-expect (blue-eyed-child-in-forest?.v3 FF1)
              (blue-eyed-child-in-forest? FF1))
(check-expect (blue-eyed-child-in-forest?.v3 FF2)
              (blue-eyed-child-in-forest? FF2))
(check-expect (blue-eyed-child-in-forest?.v3 FF3)
              (blue-eyed-child-in-forest? FF3))

(define (blue-eyed-child-in-forest?.v3 ff)
  (match ff
    [(? empty?) #f]
    [(? cons?)
     (for/or ([ft ff])
       (blue-eyed-child? ft))]))

;; NOTE: the starting point is a pair of data definitons where the second
;; refers to the first and both refer to themselves. the result is a pair
;; of fuctions where the second refers to the first and both refer to themselves.
;; in other words, the function definitions refer to each other the same way the
;; data definitons refer to each other.

;; S-EXPRESSIONS -----------------------------------------------------------------------

;; an S-expr is one of:
;; - Atom
;; - Sl

;; an SL is one of:
;; - '()
;; - (cons S-expr SL)

;; an Atom is one of:
;; - Number
;; - String
;; - Symbol

;; X -> Boolean
;; checks if X is an Atom

(check-expect (atom? 0) #t)
(check-expect (atom? "0") #t)
(check-expect (atom? 'a) #t)
(check-expect (atom? '()) #f)
(check-expect (atom? '(0 . 'a)) #f)
(check-expect (atom? #f) #f)

(define (atom? x)
  (or (number? x) (string? x) (symbol? x)))

(define (atom?.v2 x)
  (local ((define atom-predicates
            (list number? string? symbol?)))
    (ormap (lambda (proc) (proc x)) atom-predicates)))

;; [List-of X] -> Boolean
;; checks of l-x is an SL

(check-expect (sl? '()) #t)
(check-expect (sl? (list 'hello 'world)) #t)
(check-expect (sl? (list (list (list 'world) 'hello 'world) 'hello)) #t)
(check-expect (sl? (list (list (list #t) 'hello) 'hello)) #f)
(check-expect (sl? (list (list (list #t (list 'hello "world" #f #f (list (list (list 'hello))))) 'hello) 'hello)) #f)
(check-expect (sl? (list (list (list "string" (list 'hello "world" 09 90 (list (list (list '() 'hello))))) 'hello) 'hello)) #t)
(check-expect (sl? '(() () (()()()((((((((((((((((()))))))))))))))))()()()) () () ())) #t)

(define (sl? l-x)
  (cond [(empty? l-x) #t]
        [else (and (sexpr? (car l-x))
                   (sl? (cdr l-x)))]))

(define (sexpr? x)
  (or (atom? x)
      (if (list? x) (sl? x) #f)))

;; [List-of X] -> Boolean
;; like sl?, checks of l-x is an SL

(check-expect (sl?.v2 '()) #t)
(check-expect (sl?.v2 (list 'hello 'world)) #t)
(check-expect (sl?.v2 (list (list (list 'world) 'hello 'world) 'hello)) #t)
(check-expect (sl?.v2 (list (list (list #t) 'hello) 'hello)) #f)
(check-expect (sl?.v2 (list (list (list #t (list 'hello "world" #f #f (list (list (list 'hello))))) 'hello) 'hello)) #f)
(check-expect (sl?.v2 (list (list (list "string" (list 'hello "world" 09 90 (list (list (list '() 'hello))))) 'hello) 'hello)) #t)
(check-expect (sl?.v2 '(() () (()()()((((((((((((((((()))))))))))))))))()()()) () () ())) #t)

(define (sl?.v2 l-x)
  (andmap
   (lambda (x)
     (match x
       [(? empty?) #t]
       [(? cons?) (sl?.v2 x)]
       [(not (? cons?)) (atom? x)]))
   l-x))

;; S-expr Symbol -> Number
;; counts how many times some Symbol occurs in some S-expr

(check-expect (count-sexpr 'world 'hello) 0)
(check-expect (count-sexpr '(world hello) 'hello) 1)
(check-expect (count-sexpr '(((world) hello) hello) 'hello) 2)

(check-expect (count-sexpr 'hello 'hello) 1)
(check-expect (count-sexpr 20.12 'hello) 0)
(check-expect (count-sexpr "world" 'hello) 0)
(check-expect (count-sexpr '() 'hello) 0)
(check-expect (count-sexpr '(hello 20.12 "world") 'hello) 1)
(check-expect (count-sexpr '((hello 20.12 "world")) 'hello) 1)
(check-expect (count-sexpr '(define (f x) (+ x 55)) 'hello) 0)
(check-expect (count-sexpr '((6 f) (5 e) (4 d)) 'hello) 0)
(check-expect (count-sexpr '(wing (wing body wing) wing) 'hello) 0)
(check-expect (count-sexpr '(hello (hello hello hello) ((hello)) bye) 'hello) 5)

(define (count-sexpr sexpr sym)
  (local (;; Atom -> Number
          ;; counts all occurrences of sym in atom
          (define (count-atom atom)
            (cond [(number? atom) 0]
                  [(string? atom) 0]
                  [(symbol? atom) (if (symbol=? atom sym) 1 0)]))
          ;; SL -> Number
          ;; counts all occurrences of sym in sl
          (define (count-sl sl)
            (cond [(empty? sl) 0]
                  [else (+ (count-sexpr (car sl) sym)
                           (count-sl (cdr sl)))])))
    (cond [(atom? sexpr) (count-atom sexpr)]
          [else (count-sl sexpr)])))

(test)
