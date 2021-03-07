; Exercise 2.1 Arrity Repair
(define (compose f g)
  (let ((t (get-arity g)))
    (define (the-combination . args)
      (assert (= (length args) t))
      (f (apply g args)))
      (restrict-arity the-combination t)))


((compose (lambda (x) (list `foo x))
          (lambda (x) (list `bar x)))
 `z)

(define (parallel-combine h f g)
  (let ((n (get-arity f)) (m (get-arity g)))
    (define (the-combination . args)
      (assert (= (length args) n))
      (assert (= (length args) m))
      (h (apply f args) (apply g args)))
      (restrict-arity the-combination n)))

((parallel-combine list
                   (lambda (x y z) (list `foo x y z))
                   (lambda (u v w) (list `bar u v w)))
 `a `b `c)

