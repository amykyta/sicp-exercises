#lang planet neil/sicp

;(define (variable? x) (symbol? x))
;
;(define (same-variable? v1 v2)
;  (and (variable? v1) (variable? v2) (eq? v1 v2)))
;
;(define (=number? exp num)
;  (and (number? exp) (= exp num)))
;
;(define (any? seq test)
;  (cond ((null? seq) #f)
;        ((pair? seq) (or (test (car seq)) (any? (cdr seq) test)))
;        (else (test seq))))
;
;(define (remove-matching seq test)
;  (cond ((null? seq) seq)
;        ((pair? seq)
;         (if (test (car seq))
;             (remove-matching (cdr seq) test)
;             (cons (car seq) (remove-matching (cdr seq) test))))))
;
;(define (make-sum a1 a2) 
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2)) (+ a1 a2))
;        ((pair? a2) (append (list '+) 
;                            (if (sum? a2)
;                                (cons a1 (cdr a2))
;                                (cons a1 (list a2 )))))
;        (else (list '+ a1 a2))))
;
;(define (append list1 list2)
;  (if (null? list1)
;      list2
;      (cons (car list1) (append (cdr list1) list2))))
;
;;can't change the signature here..needs to still be 2 terms
;;the augend or even the compound sum/product just needs to be 
;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        ((pair? m2) (append (list '*) 
;                            (if (product? m2)
;                                (cons m1 (cdr m2))
;                                (cons m1 (list m2)))))
;        (else (list '* m1 m2))))
;
;(define (sum? x)
;  (and (pair? x) (eq? (car x) '+)))
;
;(define (addend s) (cadr s))
;
;(define (augend s)
;  (let ((aug (cddr s))
;        (two-term-sum? (null? (cdddr s))))
;     (if two-term-sum?
;         (car aug)    
;         (cons '+ aug))))
;          
;(define (product? x)
;  (and (pair? x) (eq? (car x) '*)))
;
;(define (multiplier p) (cadr p))
;
;(define (multiplicand p)
;  (let ((m (cddr p))
;        (two-term-product? (null? (cdddr p))))
;    (if two-term-product?
;        (car m)
;        (cons '* m))))
;
;(define (make-exponentiation base exp)
;  (cond ((=number? exp 0) 1)
;        ((=number? exp 1) base)
;        (else (list '** base exp))))
;
;(define (exponentiation? x)
;  (and (pair? x) (eq? (car x) '**)))
;
;(define (base x) (cadr x))
;(define (exponent x) (caddr x))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (any? seq test)
  (cond ((null? seq) #f)
        ((pair? seq) (or (test (car seq)) (any? (cdr seq) test)))
        (else (test seq))))

(define (remove-matching seq test)
  (cond ((null? seq) seq)
        ((pair? seq)
         (if (test (car seq))
             (remove-matching (cdr seq) test)
             (cons (car seq) (remove-matching (cdr seq) test))))))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (sum? a1) (sum? a2))
         (append a1 (cons '+ a2)))
        ((sum? a1)
         (append a1 (list '+ a2)))
        ((sum? a2)
         (append (list a1) (cons '+ a2)))
        
;        ((or (pair a1) (pair? a2))
;         (if (sum? a1)
;             (
;                                (append a1 (list '+ a2))
;                                (cons a1 (list a2 ))))
        (else (list a1 '+ a2))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;can't change the signature here..needs to still be 2 terms
;the augend or even the compound sum/product just needs to be 
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ((and (product? m1) (product? m2))
         (append m1 (cons '* m2)))
        ((product? m1)
         (append m1 (list '* m2)))
        ((product? m2)
         (append (list m1) (cons '* m2)))
        (else (list m1 '* m2))))

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
    (let ((aug (cddr s))
        (two-term-sum? (null? (cdddr s))))
     (if two-term-sum?
         (car aug)  
          aug)))
;      (let ((s (cddr p))
;            (two-term-sum? (null? (cdddr p))))
;       (if two-term-sum?
;           (car p)
;           p)))
          
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) 
    (let ((m (cddr p))
        (two-term-product? (null? (cdddr p))))
    (if two-term-product?
        (car m)
        m)))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        (else (list '** base exp))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base x) (cadr x))
(define (exponent x) (caddr x))

oceanside is a lot prettier than i expected in the winter

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                      (make-exponentiation (base exp)
                                                           (dec (exponent exp))))
                        (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))