; 2.84
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (not (eq? type1 type2))
                    (cond ((< (type-order type1) (type-order type2))
                           (apply-generic op (raise a1) a2))
                          ((> (type-order type1) (type-order type2)) 
                           (apply-generic op a1 (raise a2)))
                          (else
                           (error "No method for these types"
                                  (list op type-tags))))
                    (error "No method for these types" (list op type-tags))))
              ((error "Not for these types" (list op type-tags))))))))

(define (type-order type)
  (cond ((eq? type 'complex) 2)
        ((eq? type 'scheme-number) 1)
        (else 
         (display "Don't know this type's level") 
         0)))
;
; This is greatly simplified to just the two types.
; The type-order procedure is needed to determine which type is higher in the tower of types, it also
; explicitly defines our types. 
; When new types are added to the system, they need to have a 'raise procedure defined and entered
; into the table so that 'raise can be applied generically. The type-order procedure will also need
; to be updated. Doesn't seem like the best solution but that's all I've got for the time being. 
