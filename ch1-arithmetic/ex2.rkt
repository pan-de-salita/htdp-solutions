;; Exercise 2. Add the following two lines to the definitions area:
;;
;; (define prefix "hello")
;; (define suffix "world")
;;
;; The use string primitives to create an expression that concatenates prefix and
;; suffix and adds "_" between them. When you run this program, you will see
;; "hello_world" in the interactons area.

#lang htdp/bsl
(require test-engine/racket-tests)

;; string string -> string
;; Purpose: Concatenates two strings and adds "_" between them
(define (join-w-underscore str-1 str-2)
  (string-append str-1 "_" str-2))

(check-expect (join-w-underscore "hello" "world") "hello_world")
(check-expect (join-w-underscore "good night" "world") "good night_world")
(check-expect (join-w-underscore "" " ") "_ ")
(test)
