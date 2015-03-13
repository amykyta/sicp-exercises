#lang planet neil/sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(define (equal? a b)
  (cond ((and (not (pair? a))
              (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b))
             (and (equal? (car a) (car b))
                  (equal? (cdr a) (cdr b))))
        (else #f)))