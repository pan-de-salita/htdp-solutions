#lang htdp/bsl
;; assuming that "better" means acquiring the most accurate results as possible,
;; it is better to work with data definitions that accomodate non-empty lists.
;; this is because data definitions that accomodate empty lists could lead to
;; partial functions, which account for values that aren't relevant to the final
;; result of a program (e.g. what does '() mean when doing a calculation on
;; temperatues?). Data definitions that accomodate non-empty lists narrow down the
;; domain of calculation, thus producing only relevant results.
