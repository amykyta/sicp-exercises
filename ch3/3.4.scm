;3.4
(define (make-account-w-security balance password)
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
  
  (define string-of-wrong 0)
 
  (lambda (pass op)
    (if (eq? pass password)
        (begin (set! string-of-wrong 0)
               (dispatch op))
        (begin (set! string-of-wrong (+ 1 string-of-wrong))
               (if (>= string-of-wrong 7)
                   (lambda (_) (call-the-cops))
                   (lambda (_) "Incorrect password."))))))

(define (call-the-cops)
  "WEEEEUU-WEEUUU-STOP-RIGHT-THERE")

(define acc-w-security (make-account-w-security 50 'abc))
(test "3.4 - a" (eq? ((acc-w-security 'abc 'withdraw) 20) 30))
(test "3.4 - b" (eq? ((acc-w-security 'abc 'withdraw) 35) "Insufficient funds."))
(test "3.4 - c" (eq? ((acc-w-security 'abc 'deposit) 50) 80))
(test "3.4 - d" (eq? ((acc-w-security 'abcd 'withdraw) 20) "Incorrect password."))
((acc-w-security 'abcd 'withdraw) 20)
((acc-w-security 'abcd 'withdraw) 20)
((acc-w-security 'abcd 'withdraw) 20)
((acc-w-security 'abcd 'withdraw) 20)
((acc-w-security 'abcd 'withdraw) 20)
(test "3.4 - e" (eq? ((acc-w-security 'abcd 'withdraw) 20) "WEEEEUU-WEEUUU-STOP-RIGHT-THERE"))
