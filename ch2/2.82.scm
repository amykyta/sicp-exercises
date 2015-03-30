; 2.82
(define (apply-generic op . args)
  (define (try-to-coerce-all items target-type)
    (map (lambda (item) 
           (let ((coercion (get-coercion (type-tag item) target-type)))
             (if coercion
               (coercion item)
               item))) 
         items))
  (define (internal-apply-generic op args types-to-try)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (if (not (null? types-to-try))
            (internal-apply-generic op (try-to-coerce-all args (car types-to-try)) (cdr types-to-try))
            (error "No method for these types"
                   (list op type-tags)))))))
  (internal-apply-generic op args (map type-tag args)))

; if we apply a generic procedure with types 1 and 2, but there isn't a
; fitting procedure for arguments w/ type 1 or 2 but there is one for type 3
; to which both type 1 and type 2 can be coerced, it will fail regardless. 

; exp of raising a complex number to an integer power, won't happen
; even though it's just a complex multiplication. 
