#lang racket
(require test-engine/racket-tests)
(require 2htdp/image)
(require 2htdp/universe)
(require lang/posn)

(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT (/ CANVAS-WIDTH 10))
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT "black"))

(define CURSOR-WIDTH (/ CANVAS-WIDTH CANVAS-WIDTH))
(define CURSOR-HEIGHT (/ CANVAS-WIDTH 10))
(define CURSOR (rectangle CURSOR-WIDTH CURSOR-HEIGHT "solid" "red"))

(define-struct editor [pre post])
;; an Editor is a structure:
;;  (make-editor String String)
;; interpretation:
;; (make-editor s t) describes an editor
;; whose visible text is (string append s t)
;; with the cursor displayed between s and t.
(define editor-example (make-editor "hello" "world"))

;; Editor -> Image
;; interpretation: combines the two halves of
;; a (make-editor s t) into an image.
(check-expect (editor-render editor-example)
              (place-image (beside (editor->text-img (editor-pre editor-example))
                                   CURSOR
                                   (editor->text-img (editor-post editor-example)))
                           (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)
                           CANVAS))

(define (editor-render editor-entry)
  (place-image (beside (editor->text-img (editor-pre editor-entry))
                       CURSOR
                       (editor->text-img (editor-post editor-entry)))
               (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)
               CANVAS))

;; String -> Image
;; interpretation: converts an Editor field
;; entry into an Image.
(check-expect (editor->text-img (editor-pre editor-example))
              (text (editor-pre editor-example) 16 "gray"))

(define (editor->text-img editor-field-entry)
  (text editor-field-entry 16 "gray"))

(test)
