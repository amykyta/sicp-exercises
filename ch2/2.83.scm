; 2.83
(define (raise x) (apply-generic 'raise x))
; and then for each type implement a raise procedure that coerces it up one
; level of the tower
(put 'raise 'type (lambda (x) ( implement a raise to the next type )))
