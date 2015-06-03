;3.6

(define rand
  (lambda (arg)
    (cond ((eq? arg 'generate) (random))
          ((eq? arg 'reset) (lambda (new-value) (random-seed new-value)))
          (else (display "valid ways to call are (rand 'generate)") (newline)
                (display " or ((rand 'reset) new-seed)")))))