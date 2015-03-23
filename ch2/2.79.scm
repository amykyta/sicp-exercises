; 2.79
;
(define (equ? x y) (apply-generic 'equ? x y))

;in scheme-number package
  (put 'equ? '(scheme-number scheme-number)
              (lambda (x y) (= x y)))
;in complex
  (put 'equ? '(complex complex)
              (lambda (z1 z2) (and (= (real-part z1) (real-part z2)) 
                                   (= (imag-part z1) (imag-part z2)))))
; rational, same thing just compare the numerators and denominators assuming
; the numbers are reduced as much as possible. 
