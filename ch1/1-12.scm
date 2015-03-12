(define (pascals n x)
    (cond ((= n 1) 1)
          ((or (>= x n) (<= x 1)) 1)
          (else (+ (pascals (- n 1) (- x 1))
                   (pascals (- n 1) x)))))
                   