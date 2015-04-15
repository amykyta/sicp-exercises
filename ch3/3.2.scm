;3.2
(define (make-monitored proc)
  (let ((times-called 0))
    (lambda (arg)
      (if (eq? arg 'how-many-calls?)
          times-called
          (begin (set! times-called (+ 1 times-called))
                 (proc arg))))))
(define S (make-monitored sqrt))
(S 5) (S 10)
(test "3.2 - a" (eq? (S 'how-many-calls?) 2))