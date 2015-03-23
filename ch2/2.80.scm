; 2.80
;
(define (zero? x) (apply-generic 'zero? x))

;in scheme-number package
  (put 'zero? '(scheme-number)
              (lambda (x) (= x 0)))
;in complex
  (put 'zero? '(complex)
              (lambda (z) (and (= (real-part z) 0)
                               (= (imag-part z) 0))))

; rational, same thing just check the numerator
