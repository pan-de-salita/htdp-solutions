;; Exercise 25. Take a look at this attempt to solve exercise 17:
;;
;; (define (image-classify img)
;;   (cond [(>= (image-height img) (image-width img)) "tall"]
;;         [(= (image-height img) (image-width img)) "square"]
;;         [(<= (image-height img) (image-width img)) "wide"]))
;;
;; Does stepping through an application suggest a fix?

#lang racket
(require racket/trace)
(require 2htdp/image)

;; image-classify with error
 (define (image-classify img)
   (cond [(>= (image-height img) (image-width img)) "tall"]
         [(= (image-height img) (image-width img)) "square"]
         [(<= (image-height img) (image-width img)) "wide"]))

;; image-classify fixed
 (define (image-classify-fixed img)
   (cond [(> (image-height img) (image-width img)) "tall"]
         [(= (image-height img) (image-width img)) "square"]
         [(< (image-height img) (image-width img)) "wide"]))
