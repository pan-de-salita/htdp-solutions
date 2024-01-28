
(identity-matrix.v3 2)
==
(local ((define (build-identity-matrix.v2 row-number)
          (local ((define (build-row.v2 row-length)
                    (local ((define (insert-zero-or-one digit-index)
                              (if (= row-number digit-index) 1 0)))
                      (build-list row-length insert-zero-or-one))))
            (build-row.v2 n))))
  (build-list n build-identity-matrix.v2))
==
(local ((define (build-identity-matrix.v2 row-number)
          (local ((define (build-row.v2 row-length)
                    (local ((define (insert-zero-or-one digit-index)
                              (if (= row-number digit-index) 1 0)))
                      (build-list row-length insert-zero-or-one))))
            (build-row.v2 n))))
  (build-list n build-identity-matrix.v2))
