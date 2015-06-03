;3.5
(define (random-in-range low high)
  (if (and (= low 0) (= high 1))
      (random)
      (let ((range (- high low)))
        (+ low (* (random) range)))))

(define (square x) (* x x))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral experiment x1 x2 y1 y2 n-trials)
  (let ((rectangle-area (* (abs (- y2 y1)) (abs (- x2 x1))))
        (test (experiment x1 x2 y1 y2)))
    (* rectangle-area (monte-carlo n-trials test))))

; unit circle centered at (1,1) bounded by x1 to x2 and y1 to y2
(define (make-unit-circle-test x1 x2 y1 y2)
  (lambda () 
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (<= (+ (square (- x 1)) (square (- y 1))) 1))))

; enclose the unit circle centered at (1,1) with a square 
(estimate-integral make-unit-circle-test 0 2.0 0 2.0 10000)