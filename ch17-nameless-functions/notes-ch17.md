stumbled upon following function while searching for a method to apply recursion without named functions: 

```scheme
;; [X -> Y] Number X -> Y
;; applies F n times to x
(define (apply-n-times F n)
  (lambda (x) 
    (cond [(zero? n) x]
          [else (F ((apply-n-times F (sub1 n)) x))])))

(check-expect ((apply-n-times cdr 0) '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect ((apply-n-times cdr 1) '(1 2 3 4 5)) '(2 3 4 5))
(check-expect ((apply-n-times cdr 2) '(1 2 3 4 5)) '(3 4 5))
(check-expect ((apply-n-times cdr 3) '(1 2 3 4 5)) '(4 5))
```

was slightly confused by how it can take in three arguments, which was not made clear in its definition at its top level (i.e. `(define (apply-n-times F n) ...)`). perhaps this would be explained later on in ch17 of htdp? 

my alternative would be: 

```scheme
;; [X -> Y] Number X -> Y
;; applies F n times to x
(define apply-n-times.v2 
  (lambda (F n x) 
    (cond [(zero? n) x]
          [else (apply-n-times.v2 F (sub1 n) (F x))])))
          
(check-expect (apply-n-times.v2 cdr 0 '(1 2 3 4 5)) ((apply-n-times cdr 0) '(1 2 3 4 5)))
(check-expect (apply-n-times.v2 cdr 1 '(1 2 3 4 5)) ((apply-n-times cdr 1) '(1 2 3 4 5)))
(check-expect (apply-n-times.v2 cdr 2 '(1 2 3 4 5)) ((apply-n-times cdr 2) '(1 2 3 4 5)))
(check-expect (apply-n-times.v2 cdr 3 '(1 2 3 4 5)) ((apply-n-times cdr 3) '(1 2 3 4 5)))
```

-------------------

```scheme
((lambda (x-1 ... x-n) f-body) v-1 ... v-n) == f-body
;; with all occurrences of x-1 ... x-n
;; replaces with v-1 .. v-n, respecively [beta-v]
```

----------

ex284

instructed to step through the following expressions: 

[1]

```scheme
((lambda (x) x) (lambda (x) x))
```

this produces `(lambda (x) x)` because the first lambda expression is applied to the second, which itself isn't applied to any expression. 

[2]

```scheme
((lambda (x) (x x)) (lambda (x) x))
```

this also produces `(lambda (x) x)` because the first lambda expression is applied to the second, which itself applies itself to itself (just like in [1]), giving us `(lambda (x) x)`.

[3]

```scheme
((lambda (x) (x x)) (lambda (x) (x x)))
```

this expression does not terminate because as the first lambda expression is applied to the second, the second lambda expression is effectively applied to itself ad infinitum. 