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

;; Editor KeyEvent -> Editor
;; interpretation: produces another Editor
;; depending on what user inputs via their
;; keyboard. valid keyboard inputs:
;; - letters/numbers | adds input letter/number to the end of the editor pre field
;; - backspace | deletes the character immediately to the left of CURSOR
;; - left/right arrow keys | moves CURSOR one character to the left/right if applicable
(check-expect (editor-edit editor-example "a")
              (if (and (not (string=? "a" "\t"))
                       (not (string=? "a" "\r")))
                  (cond [(key=? "a" "\b") (editor-delete editor-example)]
                        [(= (string-length "a") 1) (editor-add editor-example "a")]
                        [(key=? "a" "left") (editor-left editor-example)]
                        [(key=? "a" "right") (editor-right editor-example)])
                  editor-example))

(define (editor-edit editor-entry key-event)
  (if (and (not (string=? key-event "\t"))
           (not (string=? key-event "\r")))
      (cond [(key=? key-event "\b") (editor-delete editor-entry)]
            [(= (string-length key-event) 1) (editor-add editor-entry key-event)]
            [(key=? key-event "left") (editor-left editor-entry)]
            [(key=? key-event "right") (editor-right editor-entry)])
      editor-entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auxilliary functions for editor-edit

;; String -> String
;; interpretation: deletes the character
;; immediately to the left of CURSOR.
(check-expect (editor-delete editor-example)
              (if (> (string-length (editor-pre editor-example)) 0)
                  (make-editor (string-but-last (editor-pre editor-example))
                               (editor-post editor-example))
                  editor-example))

(define (editor-delete editor-entry)
  (if (> (string-length (editor-pre editor-entry)) 0)
      (make-editor (string-but-last (editor-pre editor-entry))
                   (editor-post editor-entry))
      editor-entry))

;; String String -> String
;; interpretation: adds input letter/number to the
;; end of the editor pre field
(check-expect (editor-add editor-example "a")
              (make-editor (string-append (editor-pre editor-example) "a")
                           (editor-post editor-example)))

(define (editor-add editor-entry alphanumeric)
  (make-editor (string-append (editor-pre editor-entry) alphanumeric)
               (editor-post editor-entry)))

;; String -> String
;; interpretation: moves CURSOR one character
;; to the left if applicable
(check-expect (editor-left editor-example)
              (if (> (string-length (editor-pre editor-example)) 0)
                  (make-editor (string-but-last (editor-pre editor-example))
                               (string-append (string-only-last (editor-pre editor-example))
                                              (editor-post editor-example)))
                  editor-example))

(define (editor-left editor-entry)
  (if (> (string-length (editor-pre editor-entry)) 0)
      (make-editor (string-but-last (editor-pre editor-entry))
                   (string-append (string-only-last (editor-pre editor-entry))
                                  (editor-post editor-entry)))
      editor-entry))

;; String -> String
;; interpretation: moves CURSOR one character
;; to the right if applicable
(check-expect (editor-right editor-example)
              (if (> (string-length (editor-post editor-example)) 0)
                  (make-editor (string-append (editor-pre editor-example)
                                              (string-only-first (editor-post editor-example)))
                               (string-but-first (editor-post editor-example)))
                  editor-example))

(define (editor-right editor-entry)
  (if (> (string-length (editor-post editor-entry)) 0)
      (make-editor (string-append (editor-pre editor-entry)
                                  (string-only-first (editor-post editor-entry)))
                   (string-but-first (editor-post editor-entry)))
      editor-entry))

;; String -> String
;; interpretation: returns given String
;; with its last character removed
(check-expect (string-but-last (editor-pre editor-example))
              (substring (editor-pre editor-example)
                         0 (- (string-length (editor-pre editor-example)) 1)))

(define (string-but-last editor-field-entry)
  (substring editor-field-entry
             0 (- (string-length editor-field-entry) 1)))

;; String -> String
;; interpretation: returns the last character
;; of given String
(check-expect (string-only-last (editor-pre editor-example))
              (substring (editor-pre editor-example)
                         (- (string-length (editor-pre editor-example)) 1)
                         (string-length (editor-pre editor-example))))

(define (string-only-last editor-field-entry)
  (substring editor-field-entry
             (- (string-length editor-field-entry) 1)
             (string-length editor-field-entry)))

;; String -> String
;; interpretation: returns the first character
;; of given String
(check-expect (string-only-first (editor-pre editor-example))
              (substring (editor-pre editor-example) 0 1))

(define (string-only-first editor-field-entry)
  (substring editor-field-entry 0 1))

;; String -> String
;; interpretation: returns given String
;; with its first character removed
(check-expect (string-but-first (editor-post editor-example))
              (substring (editor-post editor-example)
                         1 (string-length (editor-post editor-example))))

(define (string-but-first editor-field-entry)
  (substring editor-field-entry
             1 (string-length editor-field-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (test)
