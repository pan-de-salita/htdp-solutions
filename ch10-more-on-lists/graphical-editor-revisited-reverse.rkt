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

;;; constants

(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 20)
(define CANVAS-COLOR "black")
(define FONT-SIZE 16)
(define FONT-COLOR "white")
(define TEXT-X-POSN (/ CANVAS-WIDTH 2))
(define TEXT-Y-POSN (/ CANVAS-HEIGHT 2))

(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT CANVAS-COLOR))
(define CURSOR (rectangle 1 CANVAS-HEIGHT "solid" "red"))

;;; functions

;; main: String -> Editor
;; launches the editor given some initial string s

(define (main s)
  (big-bang (create-editor s "")
            [on-key editor-kh]
            [to-draw editor-render]))

;; String String -> Editor
;; creates an Editor instance where the first String
;; s1 is the text to the elft of the cursor and is
;; reversed, while the second String s2 is to the
;; right and not reversed

(check-expect (create-editor "" "")
              (make-editor '() '()))
(check-expect (create-editor "left" "right")
              (make-editor (cons "t"
                                 (cons "f"
                                       (cons "e"
                                             (cons "l" '()))))
                           (cons "r"
                                 (cons "i"
                                       (cons "g"
                                             (cons "h"
                                                   (cons "t" '())))))))

(define (create-editor s1 s2)
  (make-editor (reverse (explode s1)) (explode s2)))

;; Editor -> Image
;; renders an editor e as an image of the two texts
;; separated by the cursor

(check-expect (editor-render (create-editor "pre" "post"))
              (place-image
               (beside (text "pre" FONT-SIZE FONT-COLOR)
                       CURSOR
                       (text "post" FONT-SIZE FONT-COLOR))
               TEXT-X-POSN TEXT-Y-POSN
               CANVAS))

(define (editor-render e)
  (place-image
   (beside (editor-text (reverse (editor-pre e)))
           CURSOR
           (editor-text (editor-post e)))
   TEXT-X-POSN TEXT-Y-POSN
   CANVAS))

;; Lo1s -> Image
;; renders an Lo1s as a text image

(check-expect (editor-text lo1s0)
              (text "" FONT-SIZE FONT-COLOR))
(check-expect (editor-text lo1s1)
              (text "good" FONT-SIZE FONT-COLOR))

(define (editor-text lo1s)
  (text (our-implode lo1s) FONT-SIZE FONT-COLOR))

;; Lo1s -> Text
;; simulates the implode function in htdp/bsl

(check-expect (our-implode lo1s0) "")
(check-expect (our-implode lo1s1) "good")

(define (our-implode lo1s)
  (cond [(empty? lo1s) ""]
        [else (string-append (first lo1s) (our-implode (rest lo1s)))]))

;; Editor -> Editor
;; deals with a KeyEvent ke, given some Editor e

(check-expect (editor-kh (create-editor "abc" "d") "\t")
              (create-editor "abc" "d"))
(check-expect (editor-kh (create-editor "abc" "d") "\r")
              (create-editor "abc" "d"))
(check-expect (editor-kh (create-editor "abc" "d") "left")
              (create-editor "ab" "cd"))
(check-expect (editor-kh (create-editor "" "abcd") "left")
              (create-editor "" "abcd"))
(check-expect (editor-kh (create-editor "abc" "d") "right")
              (create-editor "abcd" ""))
(check-expect (editor-kh (create-editor "abcd" "") "right")
              (create-editor "abcd" ""))
(check-expect (editor-kh (create-editor "abc" "d") "\b")
              (create-editor "ab" "d"))
(check-expect (editor-kh (create-editor "" "abcd") "\b")
              (create-editor "" "abcd"))
(check-expect (editor-kh (create-editor "abcd" "") "\b")
              (create-editor "abc" ""))
(check-expect (editor-kh (create-editor "abc" "d") "e")
              (create-editor "abce" "d"))
(check-expect (editor-kh (create-editor "" "abcd") "e")
              (create-editor "e" "abcd"))
(check-expect (editor-kh (create-editor "abcd" "") "e")
              (create-editor "abcde" ""))
(check-expect (editor-kh (create-editor "abcd" "") "shift")
              (create-editor "abcd" ""))

(define (editor-kh e ke)
  (cond [(or (key=? ke "\t") (key=? ke "\r")) e]
        [(or (key=? ke "left") (key=? ke "right")) (navigate e ke)]
        [(key=? ke "\b") (delete e)]
        [(= (string-length ke) 1) (add-character e ke)]
        [else e]))

;; Editor KeyEvent -> Editor
;; moves the cursor either one character to the left
;; or to the right, given the KeyEvent

(check-expect (navigate (create-editor "abc" "d") "left")
              (create-editor "ab" "cd"))
(check-expect (navigate (create-editor "" "abcd") "left")
              (create-editor "" "abcd"))
(check-expect (navigate (create-editor "abc" "d") "right")
              (create-editor "abcd" ""))
(check-expect (navigate (create-editor "abcd" "") "right")
              (create-editor "abcd" ""))

(define (navigate e ke)
  (cond [(key=? ke "left") (navigate-left e)]
        [(key=? ke "right") (navigate-right e)]))

;; Editor -> Editor
;; moves the cursor one character to the left

(check-expect (navigate-left (create-editor "abc" "d"))
              (create-editor "ab" "cd"))
(check-expect (navigate-left (create-editor "" "abcd"))
              (create-editor "" "abcd"))

(define (navigate-left e)
  (if (empty? (editor-pre e))
      e
      (make-editor (rest (editor-pre e))
                   (cons (first (editor-pre e)) (editor-post e)))))

;; Editor -> Editor
;; moves the cursor one character to the right

(check-expect (navigate-right (create-editor "abc" "d"))
              (create-editor "abcd" ""))
(check-expect (navigate-right (create-editor "abcd" ""))
              (create-editor "abcd" ""))

(define (navigate-right e)
  (if (empty? (editor-post e))
      e
      (make-editor (cons (first (editor-post e)) (editor-pre e))
                   (rest (editor-post e)))))

;; Editor -> Editor
;; deletes the character before the cursor

(check-expect (delete (create-editor "abc" "d"))
              (create-editor "ab" "d"))
(check-expect (delete (create-editor "" "abcd"))
              (create-editor "" "abcd"))
(check-expect (delete (create-editor "abcd" ""))
              (create-editor "abc" ""))

(define (delete e)
  (if (empty? (editor-pre e))
      e
      (make-editor (rest (editor-pre e))
                   (editor-post e))))

;; Editor KeyEvent -> Editor
;; adds the character designated to KeyEvent e to
;; the left of the cursor

(check-expect (add-character (create-editor "abc" "d") "e")
              (create-editor "abce" "d"))
(check-expect (add-character (create-editor "" "abcd") "e")
              (create-editor "e" "abcd"))
(check-expect (add-character (create-editor "abcd" "") "e")
              (create-editor "abcde" ""))
(check-expect (add-character
               (create-editor "this"
                              "string should be too long to display in our editor")
               "e")
              (create-editor "this"
                             "string should be too long to display in our editor"))

(define (add-character e ke)
  (if (editor-full? e ke)
      e
      (make-editor (cons ke (editor-pre e))
                   (editor-post e))))

;; Editor KeyEvent -> Boolean
;; checks if there is any space left to display
;; the new Editor

(check-expect (editor-full? (create-editor "abcd" "") "e")
              #false)
(check-expect (editor-full?
               (create-editor "this"
                              "string should be too long to display in our editor")
               "e")
              #true)

(define (editor-full? e ke)
  (< CANVAS-WIDTH (new-text-image-width e ke)))

;; Editor KeyEvent -> Number
;; calculates the image-width of the input Editor
;; and KeyEvent if rendered to an image

(check-expect (new-text-image-width (create-editor "abcd" "") "e") 44)
(check-expect (new-text-image-width
               (create-editor "this"
                              "string should be too long to display in our editor")
               "e")
              368)

(define (new-text-image-width e ke)
  (image-width
   (text (string-append (our-implode (editor-pre e))
                        (our-implode (editor-post e))
                        ke)
         FONT-SIZE FONT-COLOR)))

;;; application

(test)

;; (main "")
