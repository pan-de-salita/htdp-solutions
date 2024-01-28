#lang racket
(require test-engine/racket-tests)

#|

element (the name of which is "machine"):
<machine> [content] </machine>

when there is no content between, <machine /> is an acceptable shorthand

in terms of s-expression, an xml element is a named pair of parentheses that
surround some content, so the above xml element when written as an sexpr is:

'(machine)

element with one piece of content:
<machine><action /></machine>
==
<machine><action></action></machine>
==
'(machine (action))

element with three pieces of content:
<machine><action /><action /><action /></machine>
==
<machine>
  <action></action>
  <action></action>
  <action></action>
</machine>
==
'(machine (action) (action) (action))

element with one attribute and one value:
<machine initial="red"></machine>
==
'(machine ((initial "red")))

element with nexted elements that have attributes:
<machine initial="red">
  <action state="red" next="green" />
  <action state="green" next="yellow" />
  <action state="yellow" next "red" />
</machine>
==
'(machine ((initial "red"))
  (action ((state "red") (next "green")))
  (action ((state "green") (next "yellow")))
  (action ((state "yellow") (next "red"))))

;; an Xexpr.v0 (short for X-expression) is a one-item list:
;;   (cons Symbol '())
(define xexpr.v0-0 '(machine))

;; an Xexpr.v1 is a list:
;;   (cons Symbol [List-of Xexpr.v1])
(define xexpr.v1-0 '(machine (action)))
(define xexpr.v1-1 '(machine (action) (action) (action)))

;; an Xexpr.v2 is a list:
;; - (cons Symbol Body)
;; - (cons Symbol (cons [List-of Attribute] Body))
;; where Body is short for [List-of Xexpr.v2]
;; an Attribute is a list of two items:
;;   (cons Symbol (cons String '()))

|#
