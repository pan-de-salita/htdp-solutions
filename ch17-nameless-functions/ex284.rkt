#lang racket

[1]

```scheme
((lambda (x) x) (lambda (x) x))
```

this produces `(lambda (x) x)` because the first lambda expression is applied to the second, which itself isn't applied to any expression.

[2]

```scheme
((lambda (x) (x x)) (lambda (x) x))
```

this also produces `(lambda (x) x)` because the first lambda expression is applied to the second, which itself applies itself to itself (just like in [1]), giving us `(lambda (x) x)`.

[3]

```scheme
((lambda (x) (x x)) (lambda (x) (x x)))
```

this expression does not terminate because as the first lambda expression is applied to the second, the second lambda expression is effectively applied to itself ad infinitum.
