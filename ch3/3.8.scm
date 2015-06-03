;3.8
(define f
  (let ((state 0))
    (lambda (arg)
      (let ((old-state state))
        (set! state arg)
        old-state))))

(+ (f 0) (f 1)) ;i get a 1 so it's evaluation right to left