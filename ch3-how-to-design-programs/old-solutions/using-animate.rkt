#lang racket
(require 2htdp/image)
(require 2htdp/universe)

(define (picture-of-rocket height)
  (place-image (circle 5 "solid" "red")
               50 height
               (empty-scene 100 60)))
