; 2.85
;
; simplified tower of int -> real -> complex 

;at the beginning of apply-generic we try to drop all types as low as they will go
(define (apply-generic op . args)
  (let ((white-list-drop-apps '(add sub mul div)))
    ; drop args if we can
    (let ((dropped-args (if (memq op white-list-drop-apps)                        
                            (map drop args)
                            args)))
    ....)))
; then implement for each type and put into the generic procedures table
; raise and project operations
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

(put 'raise '(scheme-int)
     (lambda (x)
       (exact->inexact x)))
                               
(put 'raise '(scheme-real)
     (lambda (x) 
           (make-complex-from-real-imag x 0)))

(put 'project '(complex)
     (lambda (x) (exact->inexact (real-part x))))

(put 'project '(scheme-real)
     (lambda (x) 
           (inexact->exact (ceiling x))))
(put 'project '(scheme-int)
     (lambda (x)
       #f))

(define (drop x)
  (let ((lowered (project x)))
    (if lowered
        (let ((raised (raise lowered)))
          (if (equ? x raised)
              (drop lowered)
              x))
        x)))

;drop tests

(define (test message test-result)
  (display (if test-result 'PASS 'FAIL))
  (display " - ")
  (display message)
  (newline))

(define undroppable-complex (make-complex-from-real-imag 2 3))
(define droppable-real (make-complex-from-real-imag 2.123 0))
(define droppable-int (make-complex-from-real-imag 2.0 0))

(test "undroppable-complex" (equ? (drop undroppable-complex) undroppable-complex))
(test "droppable-complex->real" (equ? (drop droppable-real) 2.123))
(test "droppable-complex->real->int"  (equ? (drop droppable-int) 2))
(test "undroppable-real" (equ? (drop 2.34) 2.34))
(test "droppable-real->int" (equ? (drop 5.0) 5))
(test "droppable-int->int" (equ? (drop 25) 25))
