lve n)
    (/ n 2))
    
(define (square n)
    (* n n))
    
(define (remainder a b)
    (define x (- a b))
    (if (< x 0) (+ x b)
       (remainder x b)))
(define (even? n)
    (if (= (remainder n 2) 0) #t #f))
    
;; 1.16
(define (expt-iter a b n)
        (cond ((= n 0) a)
              ((even? n) (expt-iter a (square b) (/ n 2)))
              (else (expt-iter (* a b) b (- n 1)))))

(define (expt b n)
    (expt-iter 1 b n))
              

;; 1.17
(define (dumb* a b)
    (if (= b 0) 0
        (+ a (dumb* a (- b 1)))))
    
(define (better* a b)
    (cond ((= b 0) 0)
          ((even? b) (double (better* a (halve b))))
          (else (+ a (better* a (- b 1))))))
          

;; 1.18

(define (best-iter n a b)
    (cond ((= b 0) n)
          ((even? b) (best-iter n (double a) (halve b)))
          (else (best-iter (+ n a) a (- b 1)))))
              
(define (best* a b)
    (best-iter 0 a b))
