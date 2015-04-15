;;3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds."))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Uknown request -- MAKE-ACCOUNT" m))))
  (lambda (pass op)
    (if (eq? pass password)
        (dispatch op)
        (lambda (_) "Incorrect password."))))
(define acc (make-account 50 'abc))
(test "3.3 - a" (eq? ((acc 'abc 'withdraw) 20) 30))
(test "3.3 - b" (eq? ((acc 'abc 'withdraw) 35) "Insufficient funds."))
(test "3.3 - c" (eq? ((acc 'abc 'deposit) 50) 80))
(test "3.3 - d" (eq? ((acc 'abcd 'withdraw) 20) "Incorrect password."))
