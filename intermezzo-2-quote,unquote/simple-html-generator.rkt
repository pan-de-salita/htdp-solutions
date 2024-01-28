#lang racket
(require test-engine/racket-tests)
(require 2htdp/web-io)

;; String String -> ... deeply nested list ...
;; produces a web page with given author and title

(define (my-first-web-page an-author a-title)
  (show-in-browser
   `(html
     (head
      (title ,a-title)
      (meta ((http-equiv "content-type")
             (content "text-html"))))
     (body
      (h1 ,a-title)
      (p "I, " ,an-author ", made this page.")))))
