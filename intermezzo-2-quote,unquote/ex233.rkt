#lang racket
(require test-engine/racket-tests)

(check-expect `(0 ,@'(1 2 3) 4) (list 0 1 2 3 4))
(check-expect
 `(("alan" ,(* 2 500))
   ("barb" 2000)
   (,@'("carl" " , the great") 1500)
   ("dawn" 2300))
 (list (list "alan" 1000)
       (list "barb" 2000)
       (list "carl" " , the great" 1500)
       (list "dawn" 2300)))

; List-of-numbers -> ... nested list ...
; creates a row for an HTML table from l
(define (make-row l)
  (cond
    [(empty? l) '()]
    [else (cons (make-cell (first l))
                (make-row (rest l)))]))

; Number -> ... nested list ...
; creates a cell for an HTML table from a number
(define (make-cell n)
  `(td ,(number->string n)))

(check-expect
 `(html
   (body
    (table ((border "1"))
           (tr ((width "200"))
               ,@(make-row '(1 2)))
           (tr ((width "200"))
               ,@(make-row '(99 65))))))
 (list 'html
       (list 'body
             (list 'table (list (list 'border "1"))
                   (cons 'tr (cons (list (list 'width "200")) (make-row (list 1 2))))
                   (cons 'tr (cons (list (list 'width "200")) (make-row (list 99 65))))))))

(test)
