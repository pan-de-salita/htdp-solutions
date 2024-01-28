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

(define-struct editor [text cursor-index])
;; an Editor is a structure:
;;  (make-editor String Number)
;; interpretation:
;; (make-editor t ci) describes an editor
;; whose visible text is t with CURSOR
;; placed ci characters from the leftmost of t.
(define editor-example (make-editor "hello world" (string-length "hello world")))

;; Editor -> Image
;; purpose: places CURSOR within editor-text
;; and renders whole affair into an image.
(check-expect (editor-render editor-example)
              (place-image
               (beside (editor->text-img
                        (substring (editor-text editor-example)
                                   0
                                   (editor-cursor-index editor-example)))
                       CURSOR
                       (editor->text-img
                        (substring (editor-text editor-example)
                                   (editor-cursor-index editor-example))))
               (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)
               CANVAS))

(define (editor-render editor-entry)
  (place-image
   (beside
    (editor->text-img
     (substring (editor-text editor-entry)
                0
                (editor-cursor-index editor-entry)))
    CURSOR
    (editor->text-img
     (substring (editor-text editor-entry)
                (editor-cursor-index editor-entry))))
   (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)
   CANVAS))

;; String -> Image
;; purpose: converts an editor-text
;; entry into an Image.
(check-expect (editor->text-img
               (substring (editor-text editor-example)
                          0
                          (editor-cursor-index editor-example)))
              (text
               (substring (editor-text editor-example)
                          0
                          (editor-cursor-index editor-example)) 16 "gray"))

(define (editor->text-img editor-field-entry)
  (text editor-field-entry 16 "gray"))

;; Editor KeyEvent -> Editor
;; purpose: produces another Editor
;; depending on what user inputs via their
;; keyboard. valid keyboard inputs:
;; - letters/numbers | adds input letter/number to the end of the editor pre field
;; - backspace | deletes the character immediately to the left of CURSOR
;; - left/right arrow keys | moves CURSOR one character to the left/right if applicable
(check-expect (editor-edit editor-example "a") (make-editor "hello worlda" (string-length "hello worlda")))
(check-expect (editor-edit editor-example " ") (make-editor "hello worlda" (string-length "hello world ")))
(check-expect (editor-edit editor-example "\b") (make-editor "hello worl" (string-length "hello worl")))
(check-expect (editor-edit editor-example "left") (make-editor "hello world" (- (string-length "hello world") 1)))
(check-expect (editor-edit editor-example "right") (make-editor "hello world" (string-length "hello world")))
(check-expect (editor-edit (make-editor "this sentence is too long to display"
                                        (string-length "this sentence is too long to display")) "a")
              (make-editor "this sentence is too long to display"
                           (string-length "this sentence is too long to display")))
(check-expect (editor-edit editor-example "\t") editor-example)
(check-expect (editor-edit editor-example "\r") editor-example)

(define (editor-edit editor-entry key-event)
  (if (and (not (key=? key-event "\t"))
           (not (key=? key-event "\r")))
      (cond [(key=? key-event "\b") (editor-delete editor-entry)]
            [(key=? key-event "left") (editor-left editor-entry)]
            [(key=? key-event "right") (editor-right editor-entry)]
            [(editor-valid-input? editor-entry key-event)
             (editor-add editor-entry key-event)]
            [else editor-entry])
      editor-entry))

;; Editor -> String
;; purpose: deletes the character
;; immediately to the left of CURSOR.
(check-expect (editor-delete editor-example)
              (if (not (= (editor-cursor-index editor-example) 0))
                  (make-editor (string-append (substring (editor-text editor-example)
                                                         0 (- (editor-cursor-index editor-example) 1))
                                              (substring (editor-text editor-example)
                                                         (editor-cursor-index editor-example)))
                               (string-length (substring (editor-text editor-example)
                                                         0 (- (editor-cursor-index editor-example) 1))))
                  editor-example))

(define (editor-delete editor-entry)
  (if (not (= (editor-cursor-index editor-entry) 0))
      (make-editor (string-append (substring (editor-text editor-entry)
                                             0 (- (editor-cursor-index editor-entry) 1))
                                  (substring (editor-text editor-entry)
                                             (editor-cursor-index editor-entry)))
                   (string-length (substring (editor-text editor-entry)
                                             0 (- (editor-cursor-index editor-entry) 1))))
      editor-entry))

;; Editor -> String
;; purpose: moves CURSOR one character
;; to the left if applicable
(check-expect (editor-left editor-example)
              (if (and (> (string-length (editor-text editor-example)) 0)
                       (> (editor-cursor-index editor-example) 0))
                  (make-editor (editor-text editor-example)
                               (- (editor-cursor-index editor-example) 1))
                  editor-example))

(define (editor-left editor-entry)
  (if (and (> (string-length (editor-text editor-entry)) 0)
           (> (editor-cursor-index editor-entry) 0))
      (make-editor (editor-text editor-entry)
                   (- (editor-cursor-index editor-entry) 1))
      editor-entry))

;; Editor -> String
;; purpose: moves CURSOR one character
;; to the right if applicable
(check-expect (editor-right editor-example)
              (if (and (> (string-length (editor-text editor-example)) 0)
                       (< (editor-cursor-index editor-example)
                          (string-length (editor-text editor-example))))
                  (make-editor (editor-text editor-example)
                               (+ (editor-cursor-index editor-example) 1))
                  editor-example))

(define (editor-right editor-entry)
  (if (and (> (string-length (editor-text editor-entry)) 0)
           (< (editor-cursor-index editor-entry)
              (string-length (editor-text editor-entry))))
      (make-editor (editor-text editor-entry)
                   (+ (editor-cursor-index editor-entry) 1))
      editor-entry))

;; Editor KeyEvent -> Boolean
;; purpose: checks whether:
;; - KeyEvent is an acceptable 1String
;; - width of editor text does not exceed CANVAS WIDTH
;; returns #t if both are true
(check-expect (editor-valid-input? editor-example "a") #t)
(check-expect (editor-valid-input? editor-example "left") #f)
(check-expect (editor-valid-input? (make-editor "this sentence is far too long to display"
                                                (string-length "this sentence is far too long to display")) "a") #f)

(define (editor-valid-input? editor-entry key-event)
  (and (= (string-length key-event) 1)
       (<= (+ 7 (image-width (editor->text-img (editor-text editor-entry)))) CANVAS-WIDTH)))

;; Editor KeyEvent -> String
;; purpose: adds input letter/number before cursor
(check-expect (editor-add editor-example "a")
              (make-editor (string-append (substring (editor-text editor-example)
                                                     0 (editor-cursor-index editor-example))
                                          "a"
                                          (substring (editor-text editor-example)
                                                     (editor-cursor-index editor-example)))
                           (string-length (string-append (substring (editor-text editor-example)
                                                                    0 (editor-cursor-index editor-example))
                                                         "a"))))

(define (editor-add editor-entry alphanumeric)
  (make-editor (string-append (substring (editor-text editor-entry)
                                         0 (editor-cursor-index editor-entry))
                              alphanumeric
                              (substring (editor-text editor-entry)
                                         (editor-cursor-index editor-entry)
                                         (string-length (editor-text editor-entry))))
               (string-length (string-append
                               (substring (editor-text editor-entry)
                                          0 (editor-cursor-index editor-entry))
                               alphanumeric))))

;; Editor -> Editor
(define (run editor-entry)
  (big-bang editor-entry
            [to-draw editor-render]
            [on-key editor-edit]
            [check-with editor?]))

(test)
(run editor-example)
