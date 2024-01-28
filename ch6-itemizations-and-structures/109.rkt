#lang racket

(require test-engine/racket-tests)
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;;; data definitions

;; a StringPattern, when valid, is a String with the
;; following traits:
;; - starts with one "a"
;; - followed by any number of "b"s and "c"s
;; - ends with one "d"
;; NOTE: in other words, a (b|c)* d
(define ACCEPTABLE-STRING-PATTERN-0 "acbd")
(define ACCEPTABLE-STRING-PATTERN-1 "ad")

(define UNACCEPTABLE-STRING-PATTERN-0 "acbh")
(define UNACCEPTABLE-STRING-PATTERN-1 "d")

;; a ProgramMessage is one of:
;; - MESSAGE-0
;; - MESSAGE-1
;; - MESSAGE-2
;; - ERROR-MESSAGE-0
;; represents the message displayed to a user when
;; the program is running.
(define MESSAGE-0 "LEVEL 1\nexpecting 01100001.")
(define MESSAGE-1 "LEVEL 2\nexpecting 01100010, \n01100011, or 01100100.")
(define MESSAGE-2 "INPUT VALID \ngood job. exiting...")
(define ERROR-MESSAGE-0 "ERROR\ndelete then try again.")

;; a StringPatternState is a structure:
;;      (make-string-pattern-state StringPattern UserMessage)
;; represents the state of the program consisting of:
;; - the user's most recent string input
;; - the most recent program message
(define-struct string-pattern-state [user-input program-message])

(define STRING-PATTERN-STATE-TEST-0 (make-string-pattern-state "" MESSAGE-0))
(define STRING-PATTERN-STATE-TEST-1 (make-string-pattern-state "a" MESSAGE-1))
(define STRING-PATTERN-STATE-TEST-2 (make-string-pattern-state "ac" MESSAGE-1))
(define STRING-PATTERN-STATE-TEST-3 (make-string-pattern-state "acb" MESSAGE-1))
(define STRING-PATTERN-STATE-TEST-4 (make-string-pattern-state ACCEPTABLE-STRING-PATTERN-0 MESSAGE-2))
(define STRING-PATTERN-STATE-TEST-5 (make-string-pattern-state ACCEPTABLE-STRING-PATTERN-1 MESSAGE-2))
(define STRING-PATTERN-STATE-TEST-6 (make-string-pattern-state UNACCEPTABLE-STRING-PATTERN-0 ERROR-MESSAGE-0))
(define STRING-PATTERN-STATE-TEST-7 (make-string-pattern-state UNACCEPTABLE-STRING-PATTERN-1 ERROR-MESSAGE-0))

;;; constants

(define CANVAS-WIDTH 200)
(define CANVAS-HEIGHT 200)

(define CANVAS-COLOR-MESSAGE-0 "white")
(define CANVAS-COLOR-MESSAGE-1 "yellow")
(define CANVAS-COLOR-MESSAGE-2 "green")
(define CANVAS-COLOR-ERROR-MESSAGE-0 "red")

(define PROGRAM-MESSAGE-X-POSN (/ CANVAS-WIDTH 2))
(define PROGRAM-MESSAGE-Y-POSN (* 1/3 CANVAS-HEIGHT))
(define PROGRAM-MESSAGE-POSN (make-posn PROGRAM-MESSAGE-X-POSN PROGRAM-MESSAGE-Y-POSN))
(define PROGRAM-MESSAGE-FONT-SIZE (round (/ CANVAS-WIDTH 12)))
(define PROGRAM-MESSAGE-FONT-COLOR "black")

(define USER-INPUT-PENDING "")
(define USER-INPUT-X-POSN PROGRAM-MESSAGE-X-POSN)
(define USER-INPUT-Y-POSN (* 2/3 CANVAS-HEIGHT))
(define USER-INPUT-POSN (make-posn USER-INPUT-X-POSN USER-INPUT-Y-POSN))
(define USER-INPUT-FONT-SIZE (/ CANVAS-HEIGHT 5))
(define USER-INPUT-FONT-COLOR "black")

(define INITIAL-STATE STRING-PATTERN-STATE-TEST-0)

;;; functions

;; StringPatternState -> Image
;; renders the following:
;; - the most recent program message
;; - the user's most recent string input, if any
;; - CANVAS with appropriate color
(define (render-state string-pattern-state)
  (place-images
   (list (display-program-message (string-pattern-state-program-message
                                   string-pattern-state))
         (display-user-input (string-pattern-state-user-input
                              string-pattern-state)))
   (list PROGRAM-MESSAGE-POSN
         USER-INPUT-POSN)
   (empty-scene CANVAS-WIDTH CANVAS-HEIGHT
                (canvas-color (string-pattern-state-program-message string-pattern-state)))))

;; ProgramMessage -> Image
;; renders the program's most recent program message.
(check-expect (display-program-message (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-0))
              (text MESSAGE-0 PROGRAM-MESSAGE-FONT-SIZE PROGRAM-MESSAGE-FONT-COLOR))
(check-expect (display-program-message (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-1))
              (text MESSAGE-1 PROGRAM-MESSAGE-FONT-SIZE PROGRAM-MESSAGE-FONT-COLOR))
(check-expect (display-program-message (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-4))
              (text MESSAGE-2 PROGRAM-MESSAGE-FONT-SIZE PROGRAM-MESSAGE-FONT-COLOR))
(check-expect (display-program-message (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-6))
              (text ERROR-MESSAGE-0 PROGRAM-MESSAGE-FONT-SIZE PROGRAM-MESSAGE-FONT-COLOR))

(define (display-program-message program-message)
  (text (cond [(string=? program-message MESSAGE-0) MESSAGE-0]
              [(string=? program-message MESSAGE-1) MESSAGE-1]
              [(string=? program-message MESSAGE-2) MESSAGE-2]
              [(string=? program-message ERROR-MESSAGE-0) ERROR-MESSAGE-0])
        PROGRAM-MESSAGE-FONT-SIZE
        PROGRAM-MESSAGE-FONT-COLOR))

;; UserInput -> Image
;; renders the user's most recent string input, if any.
(check-expect (display-user-input (string-pattern-state-user-input STRING-PATTERN-STATE-TEST-0))
              (text "" USER-INPUT-FONT-SIZE USER-INPUT-FONT-COLOR))
(check-expect (display-user-input (string-pattern-state-user-input STRING-PATTERN-STATE-TEST-1))
              (text "a" USER-INPUT-FONT-SIZE USER-INPUT-FONT-COLOR))

(define (display-user-input user-input)
  (text user-input USER-INPUT-FONT-SIZE USER-INPUT-FONT-COLOR))

;; ProgramMessage -> Image
;; renders CANVAS with the appropriate color:
(check-expect (canvas-color (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-0))
              CANVAS-COLOR-MESSAGE-0)
(check-expect (canvas-color (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-1))
              CANVAS-COLOR-MESSAGE-1)
(check-expect (canvas-color (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-4))
              CANVAS-COLOR-MESSAGE-2)
(check-expect (canvas-color (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-6))
              CANVAS-COLOR-ERROR-MESSAGE-0)

(define (canvas-color program-message)
  (cond [(string=? program-message MESSAGE-0) CANVAS-COLOR-MESSAGE-0]
        [(string=? program-message MESSAGE-1) CANVAS-COLOR-MESSAGE-1]
        [(string=? program-message MESSAGE-2) CANVAS-COLOR-MESSAGE-2]
        [(string=? program-message ERROR-MESSAGE-0) CANVAS-COLOR-ERROR-MESSAGE-0]))

;; StringPatternState KeyEvent -> StringPatternState
;; updates current program message according to user input:
;; 1. if user hasn't entered any input, set to MESSAGE-0
;; 2. if user inputs "a" as first letter, set to MESSAGE-1, else ERROR-MESSAGE-0
;; 3. if user inputs any number of "b"s or "c"s, set to MESSAGE-1, else ERROR-MESSAGE-0
;; 4. if user inputs "d", set to MESSAGE-2, else ERROR-MESSAGE-0
(check-expect (update-state STRING-PATTERN-STATE-TEST-0 "a") STRING-PATTERN-STATE-TEST-1)
(check-expect (update-state STRING-PATTERN-STATE-TEST-1 "c") STRING-PATTERN-STATE-TEST-2)
(check-expect (update-state STRING-PATTERN-STATE-TEST-2 "b") STRING-PATTERN-STATE-TEST-3)
(check-expect (update-state STRING-PATTERN-STATE-TEST-3 "h") STRING-PATTERN-STATE-TEST-6)
(check-expect (update-state STRING-PATTERN-STATE-TEST-3 "d") STRING-PATTERN-STATE-TEST-4)
(check-expect (update-state STRING-PATTERN-STATE-TEST-3 "\b")
              (make-string-pattern-state "ac" MESSAGE-1))

(define (update-state string-pattern-state key-event)
  (make-string-pattern-state
   (key->string string-pattern-state key-event)
   (if (key=? key-event "\b")
       (main-deletion-handler string-pattern-state)
       (non-deletion-handler string-pattern-state key-event))))

;; StringPatternState KeyEvent -> UserInput
;; updates user's input according to key press.
(check-expect (key->string STRING-PATTERN-STATE-TEST-0 "a") "a")
(check-expect (key->string STRING-PATTERN-STATE-TEST-1 "b") "ab")
(check-expect (key->string STRING-PATTERN-STATE-TEST-1 "\b") "")
(check-expect (key->string STRING-PATTERN-STATE-TEST-1 "\b") "")
(check-expect (key->string STRING-PATTERN-STATE-TEST-7 "g") "d")

(define (key->string string-pattern-state key-event)
  (if (not (key=? key-event "\b"))
      (append-if-no-error string-pattern-state key-event)
      (delete-character (string-pattern-state-user-input string-pattern-state))))

;; StringPatternState KeyEvent -> UserInput
;; if ProgramMessage isn't ERROR-MESSAGE-0, appends the character entered
;; by user to existing UserInput, else leaves existing UserInput unchanged.
(check-expect (append-if-no-error STRING-PATTERN-STATE-TEST-0 "a") "a")
(check-expect (append-if-no-error STRING-PATTERN-STATE-TEST-7 "g") "d")

(define (append-if-no-error string-pattern-state key-event)
  (if (not (string=? (string-pattern-state-program-message string-pattern-state) ERROR-MESSAGE-0))
      (string-append (string-pattern-state-user-input string-pattern-state) key-event)
      (string-pattern-state-user-input string-pattern-state)))

;; UserInput -> UserInput
;; if UserInput is more than 0 characters long, deletes the last character
;; of existing UserInput, else leaves existing UserInput unchanged.
(check-expect (delete-character "abc") "ab")
(check-expect (delete-character "") "")

(define (delete-character user-input)
  (if (not (= (string-length user-input) 0))
      (substring user-input 0 (- (string-length user-input) 1))
      user-input))

;; StringPatternState -> ProgramMessage
;; updates current program message when user uses backspace to delete a character.
(check-expect (main-deletion-handler STRING-PATTERN-STATE-TEST-0) MESSAGE-0)
(check-expect (main-deletion-handler STRING-PATTERN-STATE-TEST-1)
              (valid-string-deletion-handler (string-pattern-state-user-input STRING-PATTERN-STATE-TEST-1)))
(check-expect (main-deletion-handler STRING-PATTERN-STATE-TEST-2)
              (valid-string-deletion-handler (string-pattern-state-user-input STRING-PATTERN-STATE-TEST-2)))
(check-expect (main-deletion-handler STRING-PATTERN-STATE-TEST-3)
              (valid-string-deletion-handler (string-pattern-state-user-input STRING-PATTERN-STATE-TEST-3)))
(check-expect (main-deletion-handler STRING-PATTERN-STATE-TEST-6)
              (invalid-string-deletion-handler (string-pattern-state-user-input STRING-PATTERN-STATE-TEST-6)))
(check-expect (main-deletion-handler STRING-PATTERN-STATE-TEST-7)
              (invalid-string-deletion-handler (string-pattern-state-user-input STRING-PATTERN-STATE-TEST-7)))

(define (main-deletion-handler string-pattern-state)
  (cond [(string=? (string-pattern-state-program-message string-pattern-state) MESSAGE-0)
         MESSAGE-0]
        [(string=? (string-pattern-state-program-message string-pattern-state) MESSAGE-1)
         (valid-string-deletion-handler (string-pattern-state-user-input string-pattern-state))]
        [(string=? (string-pattern-state-program-message string-pattern-state) ERROR-MESSAGE-0)
         (invalid-string-deletion-handler (string-pattern-state-user-input string-pattern-state))]))

;; UserInput -> ProgramMessage
;; checks if user's input is:
;; - one letter long -- in which case, return MESSAGE-0
;; - more than one letter long -- in which case, return MESSAGE-1
(check-expect (valid-string-deletion-handler "a") MESSAGE-0)
(check-expect (valid-string-deletion-handler "abc") MESSAGE-1)

(define (valid-string-deletion-handler user-input)
  (if (= (string-length user-input) 1)
      MESSAGE-0
      MESSAGE-1))

;; UserInput -> ProgramMessage
;; checks the following:
;; - if a second-to-the-last letter registered, checks if it is "a", "b", or "c". returns:
;;   | - MESSAGE-1 if true
;;   | - else returns ERROR-MESSAGE-0
;; - if no second-to-the-last letter registered, returns MESSAGE-0
(check-expect (invalid-string-deletion-handler "af") MESSAGE-1)
(check-expect (invalid-string-deletion-handler "abf") MESSAGE-1)
(check-expect (invalid-string-deletion-handler "abcf") MESSAGE-1)
(check-expect (invalid-string-deletion-handler "abc f") ERROR-MESSAGE-0)
(check-expect (invalid-string-deletion-handler "f") MESSAGE-0)

(define (invalid-string-deletion-handler user-input)
  (if (>= (string-length user-input) 2)
      (if (or (string=? (second-to-last-character user-input) "a")
              (string=? (second-to-last-character user-input) "b")
              (string=? (second-to-last-character user-input) "c"))
          MESSAGE-1
          ERROR-MESSAGE-0)
      MESSAGE-0))

;; String -> String
;; extracts the second-to-the-last character of a String.
(check-expect (second-to-last-character "abc") "b")

(define (second-to-last-character given-string)
  (substring given-string
             (- (string-length given-string) 2)
             (- (string-length given-string) 1)))

;; StringPatternState KeyEvent -> ProgramMessage
;; updates current program message when user doesn't use backspace to delete a character.
(check-expect (non-deletion-handler STRING-PATTERN-STATE-TEST-0 "a") MESSAGE-1)
(check-expect (non-deletion-handler STRING-PATTERN-STATE-TEST-1 "b") MESSAGE-1)
(check-expect (non-deletion-handler STRING-PATTERN-STATE-TEST-2 "c") MESSAGE-1)
(check-expect (non-deletion-handler STRING-PATTERN-STATE-TEST-3 "b") MESSAGE-1)
(check-expect (non-deletion-handler STRING-PATTERN-STATE-TEST-6 "a") ERROR-MESSAGE-0)
(check-expect (non-deletion-handler STRING-PATTERN-STATE-TEST-7 "h") ERROR-MESSAGE-0)

(define (non-deletion-handler string-pattern-state key-event)
  (cond [(or (first-user-input-a? (string-pattern-state-program-message string-pattern-state) key-event)
             (following-user-input-valid? (string-pattern-state-program-message string-pattern-state) key-event))
         MESSAGE-1]
        [(following-user-input-d? (string-pattern-state-program-message string-pattern-state) key-event)
         MESSAGE-2]
        [else ERROR-MESSAGE-0]))

;; ProgramMessage KeyEvent -> Boolean
;; checks if user's input is "a" while
;; the program message is MESSAGE-0.
(check-expect (first-user-input-a? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-0) "a") #t)
(check-expect (first-user-input-a? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-1) "a") #f)
(check-expect (first-user-input-a? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-1) "d") #f)
(check-expect (first-user-input-a? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-1) "j") #f)

(define (first-user-input-a? program-message key-event)
  (and (string=? program-message MESSAGE-0)
       (string=? key-event "a")))

;; ProgramMessage KeyEvent -> Boolean
;; checks if user's input is "b" or "c" while
;; the program message is MESSAGE-1
(check-expect (following-user-input-valid? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-2) "b") #t)
(check-expect (following-user-input-valid? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-2) "c") #t)
(check-expect (following-user-input-valid? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-2) "a") #f)
(check-expect (following-user-input-valid? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-2) "d") #f)
(check-expect (following-user-input-valid? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-3) "b") #t)
(check-expect (following-user-input-valid? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-3) "c") #t)
(check-expect (following-user-input-valid? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-3) "j") #f)
(check-expect (following-user-input-valid? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-3) " ") #f)

(define (following-user-input-valid? program-message key-event)
  (and (string=? program-message MESSAGE-1)
       (or (string=? key-event "b")
           (string=? key-event "c"))))

;; ProgramMessage KeyEvent -> Boolean
;; checks if user's input is "d" while
;; the program message is MESSAGE-1.
(check-expect (following-user-input-d? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-2) "b") #f)
(check-expect (following-user-input-d? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-2) "c") #f)
(check-expect (following-user-input-d? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-2) "a") #f)
(check-expect (following-user-input-d? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-2) "d") #t)
(check-expect (following-user-input-d? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-3) "b") #f)
(check-expect (following-user-input-d? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-3) "c") #f)
(check-expect (following-user-input-d? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-3) "j") #f)
(check-expect (following-user-input-d? (string-pattern-state-program-message STRING-PATTERN-STATE-TEST-3) "d") #t)

(define (following-user-input-d? program-message key-event)
  (and (string=? program-message MESSAGE-1)
       (string=? key-event "d")))

;; StringPatternState -> Boolean
;; checks if the program should stop running.
(check-expect (user-input-valid? STRING-PATTERN-STATE-TEST-0) #f)
(check-expect (user-input-valid? STRING-PATTERN-STATE-TEST-1) #f)
(check-expect (user-input-valid? STRING-PATTERN-STATE-TEST-2) #f)
(check-expect (user-input-valid? STRING-PATTERN-STATE-TEST-3) #f)
(check-expect (user-input-valid? STRING-PATTERN-STATE-TEST-4) #t)
(check-expect (user-input-valid? STRING-PATTERN-STATE-TEST-5) #t)
(check-expect (user-input-valid? STRING-PATTERN-STATE-TEST-6) #f)
(check-expect (user-input-valid? STRING-PATTERN-STATE-TEST-7) #f)

(define (user-input-valid? string-pattern-state)
  (and (input-conditions-met? (string-pattern-state-user-input string-pattern-state))
       (string=? (string-pattern-state-program-message string-pattern-state) MESSAGE-2)))

;; StringPattern -> Boolean
;; checks the following:
;; - if the first letter of the user's input is "a"
;; - if the last letter of the user's input is "d"
(check-expect (input-conditions-met? ACCEPTABLE-STRING-PATTERN-0) #t)
(check-expect (input-conditions-met? ACCEPTABLE-STRING-PATTERN-1) #t)
(check-expect (input-conditions-met? UNACCEPTABLE-STRING-PATTERN-0) #f)
(check-expect (input-conditions-met? UNACCEPTABLE-STRING-PATTERN-1) #f)

(define (input-conditions-met? user-input)
  (if (> (string-length user-input) 0)
      (and (string=? (substring user-input 0 1) "a")
           (string=? (substring user-input (- (string-length user-input) 1)) "d"))
      #f))

;; StringPatternState -> StringPatternState
;; main function.
(define (main string-pattern-state)
  (big-bang string-pattern-state
            [to-draw render-state]
            [on-key update-state]
            [stop-when user-input-valid? render-state]))

;;; application

(main INITIAL-STATE)
(test)
