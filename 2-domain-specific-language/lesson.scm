; (define (compose f g)
;   (define (the-composition . args)
;     (f (apply g args)))
;   the-composition)


; ((compose (lambda (x) (list `foo x))
;           (lambda (x) (list `bar x)))
;  `z)


; (define ((iterate n) f)
;   (if (= n 0)
;     identity
;     (compose f ((iterate (- n 1)) f))))

; (define (identity x) x)

; (((iterate 3) square) 5)

; (define (parallel-combine h f g)
;   (define (the-combination . args)
;     (h (apply f args) (apply g args)))
;   the-combination)

; ((parallel-combine list
;                    (lambda (x y z) (list `foo x y z))
;                    (lambda (u v w) (list `bar u v w)))
;  `a `b `c)

; (define (spread-combine h f g)
;   (let ((n (get-arity f))) (m (get-arity g))
;     (let ((t (+ n m)))
;       (define (the-combination . args)
;         (h (apply f (list-head args n))
;            (apply g (list-tail args n))))
;         (restrict-arity the-combination t))))

; ((spread-combine list
;                  (lambda (x y) (list `foo x y))
;                  (lambda (u v w) (list `bar u v w)))
;   `a `b `c `d `e)

(define arity-table (make-key-weak-eqv-hash-table))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc))) ; arity not in table
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))


; Exercise 2.1 Arrity Repair
(define (compose f g)
  (let ((t (get-arity g)))
    (define (the-combination . args)
      (assert (= (length args) t))
      (f (apply g args)))
      (restrict-arity the-combination t)))


((compose (lambda (x) (list `foo x))
          (lambda (x y z) (list `bar x y z)))
 `a `b `c)

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


