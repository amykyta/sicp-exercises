;3.1
(define (make-accumulator initial-amount)
  (lambda (new-amount)
    (begin (set! initial-amount (+ initial-amount new-amount))
           initial-amount)))

(define A (make-accumulator 5))
(test "3.1 - a" (eq? (A 5) 10))
(test "3.1 - b" (eq? (A 5) 15))
(test "3.1 - c" (eq? (A -20) -5))
