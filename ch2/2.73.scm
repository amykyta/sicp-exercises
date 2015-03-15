;2.73

;a) derive is the procedure and operators (+, *) are the types,
; the dispatching is done on those since there is only one procedure.
; variable? and number? can't be assimilated because they don't have an
; operator so no type

;b)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (install-deriv-package)
  ;; internal procedures
  
  (define (addend s) (car s))  
  (define (augend s) (cadr s))
  
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (sum-deriv exp var)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
  
  (define (multiplication-deriv exp var)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
 ;c)
  ; exponent
  (define (make-exponentiation base exp)
    (cond ((=number? exp 0) 1)
          ((=number? exp 1) base)
          (else (list '** base exp))))
  
  (define (base x) (car x))
  (define (exponent x) (cadr x))
  
  (define (exponent-deriv exp var)
         (make-product 
          (make-product (exponent exp)
                        (make-exponentiation (base exp)
                                             (make-sum (exponent exp) (- 1))))
          (deriv (base exp) var)))

  ;; interface to the rest of the system
  (put  '+ 'deriv sum-deriv)
  (put  '*  'deriv multiplication-deriv)
  (put  '**  'deriv exponent-deriv)
  'done)

(define table (make-hash))
(define get (lambda (type procedure)
              (dict-ref table type)))
(define put (lambda (type procedure implementation)
              (dict-set! table type implementation)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))


; test
(install-deriv-package)

(define (test result expected-result)
  (newline)
  (cond ((equal? result expected-result)
         (display "Good"))
        (else (display "Fail\n")
              (display "   Expected: ")
              (display expected-result)
              (newline)
              (display "   Got:      ")
              (display result))))

(test (deriv '(** x 4) 'x) 
        '(* 4 (** x 3)))

(test (deriv '(** x y) 'x)
        '(* y (** x (+ y -1))))

(test (deriv '(+ (* 3 x) y) 'x)
        '3)

(test (deriv '(** (+ (* 3 x) y) 1) 'x)
        '3)

(test (deriv '(** (+ (** x 2) 1) 2) 'x)
        '(* (* 2 (+ (** x 2) 1)) (* 2 x)))

;c) Small change to store the different differentiation procedures
; in the rows of our get/put table rather than columns
; Originally:           [+][*][**]   
;                [deriv] -  -  - 

; Now:             [deriv]
;               [+]  -
;               [*]  -
;               [**] -