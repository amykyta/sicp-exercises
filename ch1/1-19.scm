(define (fib-o n)
    (fib-iter-o 1 0 n))
    
(define (fib-iter-o a b count)
    (print a " " b " " count)
    (if (= count 0)
        b
        (fib-iter-o (+ a b) a (- count 1))))
        
(define (even? n)
    (= (remainder n 2) 0))

(define (remainder x y)
    (cond ((= x y) 0)
          ((< (abs x) y) x)
          (else (remainder (- x y) y))))
          
(define (fib n)
  (fib-iter 1 0 0 1 n))
  
(define (fib-iter a b p q count)
    (print a " " b " " p " " q " " count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))      ; compute p'
                   (+ (* q q) (* 2 p q))    ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))