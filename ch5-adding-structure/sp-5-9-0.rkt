#lang htdp/bsl
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

;; a space-game is a structure:
;;  (make-space-game Posn Number).
;; interpretation:
;; (make-space-game (make-posn UFO-x UFO-y) TANK-x)
;; describes a configuration where the UFO is
;; at (UFO-x, UFO-y) and the tank's x-coordinate
;; is TANK-x.
(define-struct space-game [ufo tank])
