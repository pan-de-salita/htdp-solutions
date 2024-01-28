;; 1.
;; (lambda (x y) (x y y)) is legal if x is a function that accepts 2 or more arguments.
;; 2.
;; (lambda () 10) is illegal because no parameter is specified.
;; 3.
;; (lambda (x) x) is legal because it returns x.
;; 4.
;; (lambda (x y) x) is legal because it returns x albeit not performing any operation on y.
;; 5.
;; (lambda x 10) is illegal because parameters must be specified in parentheses.
