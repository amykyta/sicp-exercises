    
(define (smallest-divisor n)
    (find-divisor n 2))
    
(define (find-divisor n test-divisor)
    (cond 
         ((> (square test-divisor) n) n) 
         ((divides? test-divisor n) test-divisor)
         (else (find-divisor
                n
                (+ test-divisor 1)))))
                
(define (divides? a b)
    (= (remainder b a) 0))

(define (prime? n)
	(= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (* 10000 (- (runtime) start-time)))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 10)

(define (search_for_primes n)
	(search-iter n 3))

(define (search-iter n count)
	(newline)
	(define p (get-next-prime n (current-inexact-milliseconds)))
	(display p)
	(cond ((= count 1))
		  (else 
		  	(search-iter (+ p 1) (- count 1)))))

(define (get-next-prime n start_time)
	(if (even? n) 
		(get-next-prime (+ n 1) start_time)
		(cond ((prime? n) 
				(report-prime (- (current-inexact-milliseconds) start_time))
				n)
			(else (get-next-prime (+ 2 n) start_time)))))

(search_for_primes 10000)
(search_for_primes 100000)
(search_for_primes 1000000)