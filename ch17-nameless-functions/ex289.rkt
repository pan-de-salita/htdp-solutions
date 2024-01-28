#lang racket
(require test-engine/racket-tests)

;; String [List-of String] -> Boolean
;; checks whether any of the Strings in l-name are
;; equal to or an extension of reference-name

(check-expect (find-name "a" '()) #f)
(check-expect (find-name "a" '("a")) #t)
(check-expect (find-name "a" '("c" "a" "b")) #t)
(check-expect (find-name "a" '("d" "e" "f")) #f)
(check-expect (find-name "TODDLE" '("czhgjf" "dfkTODDLEuir" "fjkdj")) #t)
(check-expect (find-name "TODDLE" '("czhgjf" "klasdjfg" "fjkdj")) #f)

(define (find-name reference-name l-name)
  ;; String Number -> [List-of String]
  ;; returns a [List-of String] containing substrings of
  ;; a-string of length (sub1 length-of-reference-name)
  ;; e.g. (substrings-of-length-n "dfkTODDLEuir" 6)
  ;;      == (list "dfkTOD" "fkTODD" "kTODDL" "TODDLE" "ODLLEu" "DLLEui" "LLEuir")
  (define (substrings-of-length-n a-string ending-index)
    (cond [(or (< (string-length a-string) ending-index)
               (string=? (substring a-string ending-index) ""))
           (list a-string)]
          [else (cons (substring a-string 0 ending-index)
                      (substrings-of-length-n (substring a-string 1) ending-index))]))
  (ormap
   (lambda (a-name)
     (not (boolean?
           (member reference-name
                   (substrings-of-length-n a-name (string-length reference-name))))))
   l-name))

;; String [List-of String] -> Boolean
;; like find-name, checks whether any of the Strings in l-name are
;; equal to or an extension of reference-name

(check-expect (find-name.v2 "a" '()) #f)
(check-expect (find-name.v2 "a" '("a")) #t)
(check-expect (find-name.v2 "a" '("c" "a" "b")) #t)
(check-expect (find-name.v2 "a" '("d" "e" "f")) #f)
(check-expect (find-name.v2 "TODDLE" '("czhgjf" "dfkTODDLEuir" "fjkdj")) #t)
(check-expect (find-name.v2 "TODDLE" '("czhgjf" "klasdjfg" "fjkdj")) #f)

(define (find-name.v2 reference-name l-name)
  (define length-of-reference-name (string-length reference-name))
  ;; String -> [List-of String]
  ;; returns a [List-of String] containing substrings of
  ;; a-string of length (sub1 length-of-reference-name)
  ;; e.g. (substrings-of-length-n "dfkTODDLEuir" 6)
  ;;      == (list "dfkTOD" "fkTODD" "kTODDL" "TODDLE" "ODLLEu" "DLLEui" "LLEuir")
  (define (substrings-of-length-n a-string)
    (cond [(or (< (string-length a-string) length-of-reference-name)
               (string=? (substring a-string 0 length-of-reference-name) ""))
           (list a-string)]
          [else (cons (substring a-string 0 length-of-reference-name)
                      (substrings-of-length-n (substring a-string 1)))]))
  (ormap (lambda (l-substring) (not (boolean? (member reference-name l-substring))))
         (map substrings-of-length-n l-name)))

;; String [List-of String] -> Boolean
;; like fine-name, checks whether any of the Strings in l-name are
;; equal to or an extension of reference-name

(check-expect (find-name.v3 "a" '()) #f)
(check-expect (find-name.v3 "a" '("a")) #t)
(check-expect (find-name.v3 "a" '("c" "a" "b")) #t)
(check-expect (find-name.v3 "a" '("d" "e" "f")) #f)
(check-expect (find-name.v3 "TODDLE" '("czhgjf" "dfkTODDLEuir" "fjkdj")) #t)
(check-expect (find-name.v3 "TODDLE" '("czhgjf" "klasdjfg" "fjkdj")) #f)

(define (find-name.v3 ref l-name)
  (local (;; String -> Boolean
          ;; checks if name-from-list is equal to or
          ;; is an extension of ref
          (define (own-contains? name-from-list)
            (cond [(or (< (string-length name-from-list) (string-length ref)) (string=? name-from-list "")) #f]
                  [else (or (string=? (substring name-from-list 0 (string-length ref)) ref)
                            (own-contains? (substring name-from-list 1)))])))
    (ormap own-contains? l-name)))

;; String [List-of String] -> Boolean
;; like fine-name, checks whether any of the Strings in l-name are
;; equal to or an extension of reference-name

(check-expect (find-name.v4 "a" '()) (find-name.v3 "a" '()))
(check-expect (find-name.v4 "a" '("a")) (find-name.v3 "a" '("a")))
(check-expect (find-name.v4 "a" '("c" "a" "b")) (find-name.v3 "a" '("c" "a" "b")))
(check-expect (find-name.v4 "a" '("d" "e" "f")) (find-name.v3 "a" '("d" "e" "f")))
(check-expect (find-name.v4 "TODDLE" '("czhgjf" "dfkTODDLEuir" "fjkdj"))
              (find-name.v3 "TODDLE" '("czhgjf" "dfkTODDLEuir" "fjkdj")))
(check-expect (find-name.v4 "TODDLE" '("czhgjf" "klasdjfg" "fjkdj"))
              (find-name.v3 "TODDLE" '("czhgjf" "klasdjfg" "fjkdj")))
(check-expect (find-name.v4 "TODDLE" '("miggy" "is" "fjlkasdjfasldkTODDLEfjslakdfjalsdkfj"))
              (find-name.v3 "TODDLE" '("miggy" "is" "fjlkasdjfasldkTODDLEfjslakdfjalsdkfj")))

(define (find-name.v4 ref l-name)
  (ormap
   (lambda (name-from-list)
     (not (boolean?
           (member ref
                   (cond [(<= (string-length name-from-list) (string-length ref))
                          (list name-from-list)]
                         [else (build-list
                                (add1 (- (string-length name-from-list) (string-length ref)))
                                ;; the above formula determines:
                                ;; - with 1 subtracted, the number of substrings to build
                                ;; - with starting-index added, each ending index for when name-of-list is sliced
                                (lambda (starting-index)
                                  (substring name-from-list
                                             starting-index
                                             (+ starting-index (string-length ref)))))])))))
   l-name))

;; [List-of String] -> Boolean
;; checks if all names on a list of names start will "a"

(check-expect (all-start-with-a? '()) #t) ;; #f is default value when list is empty
(check-expect (all-start-with-a? '("a" "ab" "abc")) #t)
(check-expect (all-start-with-a? '("a" "bac" "ab" "abc" "cba")) #f)

(define (all-start-with-a? l-name)
  (andmap (lambda (name) (string=? (substring name 0 1) "a")) l-name))

;; [List-of String] Number -> Boolean
;; checks if no String on l-name exceeds n

(check-expect (name-within-bounds?/and '("a" "ab" "abc" "abcd") 4) #t)
(check-expect (name-within-bounds?/and '("a" "ab" "abc" "abcd" "abcde") 4) #f)

(define (name-within-bounds?/and l-name n)
  (andmap (lambda (a-name) (<= (string-length a-name) n)) l-name))

;; [List-of String] Number -> Boolean
;; like name-within-bounds?/or, checks if no String on l-name exceeds n

(check-expect (name-within-bounds?/or '("a" "ab" "abc" "abcd") 4)
              (name-within-bounds?/and '("a" "ab" "abc" "abcd") 4))
(check-expect (name-within-bounds?/or '("a" "ab" "abc" "abcd" "abcde") 4)
              (name-within-bounds?/and '("a" "ab" "abc" "abcd" "abcde") 4))

(define (name-within-bounds?/or l-name n)
  (not (ormap (lambda (a-name) (> (string-length a-name) n)) l-name)))

#|

NOTE:
(> (string-length a-name) n) instead of (>= (string-length a-name) n) was
used for name-with-bounds?/on because, given two strings a and b, if a were
not longer than b, the length of a would either be smaller or equal to b,
which is, in other words, the logical inverse of (<= (string-length a-name) n)

|#

(test)

;; test for find-name.v4:

;; (build-list
;;  (add1 (- (string-length "fjlkasdjfasldkTODDLEfjslakdfjalsdkfj") (string-length "TODDLE")))
;;  ;; the above formula determines:
;;  ;; - with 1 subtracted, the number of substrings to build
;;  ;; - with starting-index added, each ending index for when name-of-list is sliced
;;  (lambda (starting-index)
;;    (substring "fjlkasdjfasldkTODDLEfjslakdfjalsdkfj"
;;               starting-index
;;               (+ starting-index (string-length "TODDLE")))))
