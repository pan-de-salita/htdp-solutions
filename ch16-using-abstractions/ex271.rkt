#lang racket
(require test-engine/racket-tests)

;; String [List-of String] -> Boolean
;; checks whether the following holds for
;; any of the names on l-name:
;; - is equal to a-name
;; - is an extension of a-name (that is,
;;   the strings in a-name figures in the
;;   same order in any string on l-name)

(check-expect (find-name "abc" '()) #f)
;; NOTE: if lst is empty when ormap is applied, #f is returned.
(check-expect (find-name "abc" '("abc")) #t)
(check-expect (find-name "abc" '("abcdef")) #t)
(check-expect (find-name "abc" '("defabcghi")) #t)
(check-expect (find-name "abc" '("defacabbabcccghi")) #t)
(check-expect (find-name "abc" '("a" "b" "c" "d" "e" "f")) #f)
(check-expect (find-name "abc" '("abc" "def" "ghi")) #t)
(check-expect (find-name "abc" '("def" "abc" "ghi")) #t)
(check-expect (find-name "abc" '("def" "ghi" "abc")) #t)
(check-expect (find-name "abc" '("a" "b" "c" "def" "ghi")) #f)
(check-expect (find-name "abc" '("defghi" "j" "defacabbabcccghi" "k" "xabcyz")) #t)

(define (find-name a-name l-name)
  (local (;; String String -> Boolean
          ;; checks whether the following is true:
          ;; - name-from-list is equal to a-name
          ;; - name-from-list contains a-name
          (define (name-found? name-from-list)
            (local (;; String String -> Boolean
                    ;; checks if name-from-list contains a-name
                    (define (own-string-contains? name-from-list)
                      (cond [(or (< (string-length name-from-list) (string-length a-name))
                                 (string=? name-from-list "")) #f]
                            [else (or (string=? a-name (substring name-from-list 0 (string-length a-name)))
                                      (own-string-contains? (substring name-from-list 1)))])))
              (or (string=? a-name name-from-list)
                  (own-string-contains? name-from-list)))))
    (ormap name-found? l-name)))

;; String [List-of String] -> Boolean
;; find-name.v2 works just like find-name

(check-expect (find-name.v2 "abc" '()) #f)
(check-expect (find-name.v2 "abc" '("abc")) #t)
(check-expect (find-name.v2 "abc" '("abcdef")) #t)
(check-expect (find-name.v2 "abc" '("defabcghi")) #t)
(check-expect (find-name.v2 "abc" '("defacabbabcccghi")) #t)
(check-expect (find-name.v2 "abc" '("a" "b" "c" "d" "e" "f")) #f)
(check-expect (find-name.v2 "abc" '("abc" "def" "ghi")) #t)
(check-expect (find-name.v2 "abc" '("def" "abc" "ghi")) #t)
(check-expect (find-name.v2 "abc" '("def" "ghi" "abc")) #t)
(check-expect (find-name.v2 "abc" '("a" "b" "c" "def" "ghi")) #f)
(check-expect (find-name.v2 "abc" '("defghi" "j" "defacabbabcccghi" "k" "xabcyz")) #t)

(define (find-name.v2 a-name l-name)
  (local (;; String -> Boolean
          ;; checks if name-from-string figures in a-name
          (define (name-found?.v2 name-from-list) (string-contains? name-from-list a-name)))
    (ormap name-found?.v2 l-name)))

;; String [List-of String] -> Boolean
;; find-name.v3 works just like find-name

(check-expect (find-name.v3 "abc" '()) #f)
(check-expect (find-name.v3 "abc" '("abc")) #t)
(check-expect (find-name.v3 "abc" '("abcdef")) #t)
(check-expect (find-name.v3 "abc" '("defabcghi")) #t)
(check-expect (find-name.v3 "abc" '("defacabbabcccghi")) #t)
(check-expect (find-name.v3 "abc" '("a" "b" "c" "d" "e" "f")) #f)
(check-expect (find-name.v3 "abc" '("abc" "def" "ghi")) #t)
(check-expect (find-name.v3 "abc" '("def" "abc" "ghi")) #t)
(check-expect (find-name.v3 "abc" '("def" "ghi" "abc")) #t)
(check-expect (find-name.v3 "abc" '("a" "b" "c" "def" "ghi")) #f)
(check-expect (find-name.v3 "abc" '("defghi" "j" "defacabbabcccghi" "k" "xabcyz")) #t)

(define (find-name.v3 a-name l-name)
  (ormap (lambda (name-from-list) (string-contains? name-from-list a-name)) l-name))

;; [List-of String] -> Boolean
;; checks if all names of a list of names starts with a-letter

(check-expect (starts-with? "a" '()) #t)
;; NOTE: if lst is empty when andmap is applied, #t is returned.
(check-expect (starts-with? "a" '("a" "ab" "abc")) #t)
(check-expect (starts-with? "a" '("a" "b" "c")) #f)

(define (starts-with? a-letter l-name)
  (local (;; String -> Boolean
          ;; checks if name-from-list begins with a-letter
          (define (starts-with-a-letter? name-from-list)
            (string=? a-letter (substring name-from-list 0 1))))
    (andmap starts-with-a-letter? l-name)))

;; when defining a function that ensures no name on some list exceeds
;; a given length, either andmap or ormap can be used.

;; [List-of String] -> Boolean
;; returns #t if all names on a list of names under a-length

(check-expect (under-width? '() 3) #t)
(check-expect (under-width? '("ab" "de" "fg") 3) #t)
(check-expect (under-width? '("ab" "efg" "hijklmnop") 3) #f)
(check-expect (under-width? '("ab" "efghijk" "lm") 3) #f)
(check-expect (under-width? '("abcdefghijklmn" "op" "qrs") 3) #f)

(define (under-width? l-name a-length)
  (local (;; String -> Boolean
          ;; checks if a name is within or under a-length
          (define (within-width?/per-name name-from-list)
            (< (string-length name-from-list) a-length)))
    (andmap within-width?/per-name l-name)))

;; [List-of String] -> Boolean
;; under-width?.v2 works just like under-width?

(check-expect (under-width?.v2 '() 3) #t)
(check-expect (under-width?.v2 '("abc" "efg" "hij") 3) #t)
(check-expect (under-width?.v2 '("abc" "efg" "hijklmnop") 3) #f)
(check-expect (under-width?.v2 '("abc" "efghijk" "lmn") 3) #f)
(check-expect (under-width?.v2 '("abcdefghijklmn" "op" "qrs") 3) #f)

(define (under-width?.v2 l-name a-length)
  (not (ormap (lambda (name-from-list) (> (string-length name-from-list) a-length)) l-name)))

(test)
