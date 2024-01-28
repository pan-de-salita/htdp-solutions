#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)

;;; data definitions

;; an Lo1s is one of:
;; - '()
;; - (cons 1String Lo1S)

(define lo1s0 '())
(define lo1s1 (cons "g" (cons "o" (cons "o" (cons "d" '())))))
(define lo1s2 (cons "a" (cons "l" (cons "l" '()))))
(define lo1s3 (cons "l" (cons "l" (cons "a" '()))))

(define-struct editor [pre post])
;; an Editor is a structure:
;;     (make-editor Lo1S Lo1S)
;; i.e. (make-editor pre post) makes up
;; the entire text to display in our program,
;; with the cursor shown between pre and post

(define editor0 (make-editor lo1s2 lo1s1))
(define editor1 (make-editor lo1s3 lo1s1))

;;; functions

;; String String -> Editor
;; creates an Editor instance where the first String
;; s1 is the text to the left of the cursor and the
;; second String s2 is to the right

(check-expect (create-editor0 "" "")
              (make-editor '() '()))
(check-expect (create-editor0 "left" "right")
              (make-editor (cons "l"
                                 (cons "e"
                                       (cons "f"
                                             (cons "t" '()))))
                           (cons "r"
                                 (cons "i"
                                       (cons "g"
                                             (cons "h"
                                                   (cons "t" '())))))))

(define (create-editor0 s1 s2)
  (make-editor (explode s1) (explode s2)))

;; String String -> Editor
;; creates an Editor instance where the first String
;; s1 is the text to the elft of the cursor and is
;; reversed, while the second String s2 is to the
;; right and not reversed

(check-expect (create-editor1 "" "")
              (make-editor '() '()))
(check-expect (create-editor1 "left" "right")
              (make-editor (cons "t"
                                 (cons "f"
                                       (cons "e"
                                             (cons "l" '()))))
                           (cons "r"
                                 (cons "i"
                                       (cons "g"
                                             (cons "h"
                                                   (cons "t" '())))))))

(define (create-editor1 s1 s2)
  (make-editor (reverse (explode s1)) (explode s2)))

;; Lo1s -> Lo1s
;; produces a reverse version of the given Lo1s

(check-expect (rev '()) '())
(check-expect (rev (cons "a" '())) (cons "a" '()))
(check-expect (rev lo1s3) lo1s2)

(define (rev lo1s)
  (cond [(empty? lo1s) '()]
        [else (add-to-end (rev (rest lo1s)) (first lo1s))]))

;; Lo1s 1String -> Lo1s
;; produces a new list with the given 1String s
;; added to the end of the given Lo1s

(check-expect (add-to-end '() "a") (cons "a" '()))
(check-expect (add-to-end lo1s1 "y")
              (cons "g"
                    (cons "o"
                          (cons "o"
                                (cons "d"
                                      (cons "y" '()))))))

(define (add-to-end lo1s s)
  (cond [(empty? lo1s) (cons s '())]
        [else (cons (first lo1s) (add-to-end (rest lo1s) s))]))

;;; application

(test)
