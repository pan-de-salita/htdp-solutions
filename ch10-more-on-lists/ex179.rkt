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

(define WIDTH 200)
(define HEIGHT 20)
(define FONT-SIZE 16)
(define FONT-COLOR "white")

(define CANVAS (empty-scene WIDTH HEIGHT "dimgrey"))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))

;;; functions

;; main: String -> Editor
;; launches the editor given some initial string

(define (main s)
  (big-bang (create-editor s "")
            [on-key editor-kh]
            [to-draw editor-render]))

;; String String -> Editor
;; creates an Editor instance where the first String
;; s1 is the text to the left of the cursor and the
;; second String s2 is to the right

(check-expect (create-editor "" "")
              (make-editor '() '()))
(check-expect (create-editor "left" "right")
              (make-editor (cons "l"
                                 (cons "e"
                                       (cons "f"
                                             (cons "t" '()))))
                           (cons "r"
                                 (cons "i"
                                       (cons "g"
                                             (cons "h"
                                                   (cons "t" '())))))))

(define (create-editor s1 s2)
  (make-editor (explode s1) (explode s2)))

;; Editor -> Image
;; renders an editor e as an image of the two texts
;; separated by the cursor

(define (editor-render e) CANVAS)

;; Editor KeyEvent -> Editor
;; deals with a KeyEvent ke, given some editor e

(check-expect (editor-kh (create-editor "" "") "\t")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "" "") "\r")
              (create-editor "" ""))
(check-expect (editor-kh (create-editor "cd" "fgh") "\t")
              (create-editor "cd" "fgh"))
(check-expect (editor-kh (create-editor "cd" "fgh") "\r")
              (create-editor "cd" "fgh"))
(check-expect (editor-kh (create-editor "" "") "\b")
              (delete (create-editor "" "")))
(check-expect (editor-kh (create-editor "cd" "fgh") "\b")
              (delete (create-editor "cd" "fgh")))
(check-expect (editor-kh (create-editor "" "fgh") "\b")
              (delete (create-editor "" "fgh")))
(check-expect (editor-kh (create-editor "cd" "") "\b")
              (delete (create-editor "cd" "")))
(check-expect (editor-kh (create-editor "" "") "left")
              (navigate (create-editor "" "") "left"))
(check-expect (editor-kh (create-editor "cd" "fgh") "left")
              (navigate (create-editor "cd" "fgh") "left"))
(check-expect (editor-kh (create-editor "" "") "right")
              (navigate (create-editor "" "") "right"))
(check-expect (editor-kh (create-editor "cd" "fgh") "right")
              (navigate (create-editor "cd" "fgh") "right"))
(check-expect (editor-kh (create-editor "" "") "e")
              (add-character "" "" "e"))
(check-expect (editor-kh (create-editor "cd" "fgh") "e")
              (add-character "cd" "fgh" "e"))

(define (editor-kh e ke)
  (cond [(or (key=? ke "\t") (key=? ke "\r")) e]
        [(key=? ke "\b") (delete e)]
        [(or (key=? ke "left") (key=? ke "right")) (navigate e ke)]
        [else (add-character (implode (editor-pre e))
                             (implode (editor-post e))
                             ke)]))

;; Editor -> Editor
;; deletes the character before the cursor

(check-expect (delete (create-editor "" ""))
              (create-editor "" ""))
(check-expect (delete (create-editor "all" ""))
              (create-editor "al" ""))
(check-expect (delete (create-editor "" "good"))
              (create-editor "" "good"))
(check-expect (delete editor0)
              (create-editor "al" "good"))

(define (delete e)
  (if (empty? (editor-pre e))
      e
      (make-editor (delete-char (editor-pre e)) (editor-post e))))

;; Lo1s -> Lo1s
;; deletes the last character of a given Lo1s
;; NOTE: the input Lo1s won't be empty

(check-expect (delete-char lo1s1)
              (cons "g" (cons "o" (cons "o" '()))))

(define (delete-char lo1s)
  (cond [(empty? (rest lo1s)) '()]
        [else (cons (first lo1s) (delete-char (rest lo1s)))]))

;; Editor KeyEvent -> Editor
;; moves the cursor in the given Editor e one character
;; to either the left or right according to the KeyEvent

(check-expect (navigate editor0 "left")
              (create-editor "al" "lgood"))
(check-expect (navigate editor0 "right")
              (create-editor "allg" "ood"))
(check-expect (navigate (create-editor "" "allgood") "left")
              (create-editor "" "allgood"))
(check-expect (navigate (create-editor "allgood" "") "right")
              (create-editor "allgood" ""))

(define (navigate e ke)
  (cond [(key=? ke "left") (navigate-left e)]
        [(key=? ke "right") (navigate-right e)]))

;; Editor -> Editor
;; moves the cursor in the given Editor e one character
;; to the left

(check-expect (navigate-left editor0)
              (create-editor "al" "lgood"))
(check-expect (navigate-left (create-editor "" "allgood"))
              (create-editor "" "allgood"))

(define (navigate-left e)
  (cond [(empty? (editor-pre e)) e]
        [else (move-cursor-left (implode (editor-pre e))
                                (implode (editor-post e)))]))

;; String String -> Editor
;; moves the last character of the first String s1 to
;; the beginning of the second String s2

(check-expect (move-cursor-left "all" "good") (create-editor "al" "lgood"))

(define (move-cursor-left s1 s2)
  (create-editor (substring s1 0 (sub1 (string-length s1)))
                 (string-append (substring s1 (sub1 (string-length s1))) s2)))

;; Editor -> Editor
;; moves the cursor in the given Editor e one character
;; to the right

(check-expect (navigate-right editor0)
              (create-editor "allg" "ood"))
(check-expect (navigate-right (create-editor "allgood" ""))
              (create-editor "allgood" ""))

(define (navigate-right e)
  (cond [(empty? (editor-post e)) e]
        [else (move-cursor-right (implode (editor-pre e))
                                 (implode (editor-post e)))]))

;; String String -> Editor
;; moves the first character of the second String s2 to
;; the end of the first String s1

(check-expect (move-cursor-right "all" "good") (create-editor "allg" "ood"))

(define (move-cursor-right s1 s2)
  (create-editor (string-append s1 (substring s2 0 1))
                 (substring s2 1 (string-length s2))))

;; String String KeyEvent -> Editor
;; adds the KeyEvent to the end of the first String s1

(check-expect (add-character "all" "good" " ")
              (create-editor " all" "good"))

(define (add-character s1 s2 ke)
  (create-editor (string-append ke s1) s2))

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

;;; misc.

;; String String -> Editor
;; creates an Editor instance where the first String
;; s1 is the text to the elft of the cursor and is
;; reversed, while the second String s2 is to the
;; right and not reversed

;; (check-expect (create-editor1 "" "")
;;               (make-editor '() '()))
;; (check-expect (create-editor1 "left" "right")
;;               (make-editor (cons "t"
;;                                  (cons "f"
;;                                        (cons "e"
;;                                              (cons "l" '()))))
;;                            (cons "r"
;;                                  (cons "i"
;;                                        (cons "g"
;;                                              (cons "h"
;;                                                    (cons "t" '())))))))

;; (define (create-editor1 s1 s2)
;;   (make-editor (reverse (explode s1)) (explode s2)))
