(define M (- (expt 2 31) 1))

(define (! n)
  (define (!-iter n acc)
    (if (zero? n)
      acc
      (!-iter (- n 1) (modulo (* acc n) M))
    )
  )
  (!-iter n 1)
)

(! 10000000)
