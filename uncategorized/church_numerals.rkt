#lang planet neil/sicp

;(define (cons x y)
;  (lambda (m) (m x y)))
;
;(define (car z)
;  (z (lambda (p q) p)))
;
;(define (cdr z)
;  (z (lambda (p q) q)))
;
;(define a (cons 3 5))
;(car a)
;(cdr a)

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (cdr z)
    (define (get-b z a b)
;      (newline)
;      (display z)
;      (display " ")
;      (display a)
;      (display " ")
;      (display b)
    (if (= z 1)
        b
        (if (even? z)
            (get-b (/ z 2) (inc a) b)
            (get-b (/ z 3) a (inc b)))))
  (get-b z 0 0))

(define (car z)
  (define (get-a z a b)
    (if (= z 1)
        a
        (if (even? z)
            (get-a (/ z 2) (inc a) b)
            (get-a (/ z 3) a (inc b)))))
  (get-a z 0 0))


;(define x (cons 25 17))
;(car x)
;(cdr x)
 
(define zero 
  (lambda (f) 
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f) 
    (lambda (x) 
      (f ((n f) x)))))

(add-1 zero)

(define one 
  (lambda (f)
    (lambda (x)
      (f x))))

(define two 
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (plus a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

(define (count f)
  ((f inc) 0))

(count zero)
(count one)
(count two)
(count (plus one two))

;(lambda (f)
;  (lambda (x)
;    
;    (lambda (f)
;      (lambda (x)
;        (f x)))
;      f) (f (f x)))
