#lang planet neil/sicp
;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

;(paint-hires  (below (beside einstein
;                             (rotate90 einstein))
;                     (beside (rotate270 einstein)
;                             (rotate180 einstein))))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-split painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;(define (split first-order second-order)
;  (lambda (painter n)
;    (define (recursive-split painter n)
;;      (newline)
;;      (display n)
;      (if (= n 0)
;          painter
;          (let ((smaller (recursive-split painter (- n 1))))
;            (first-order painter (second-order painter painter)))))
;    (recursive-split painter n)))
;
;(define right-split (split beside below))
;
;(define up-split (split below beside))

;(paint-hires (corner-split einstein 5))

;(paint-hires (right-split einstein 5))
;(paint-hi-res (right-split einstein 5))

;(paint-hires (up-split einstein 10))

;(paint-hires (square-split einstein 15))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

(define (comb-vectors combiner)
   (lambda (v1 v2)
     (make-vect (combiner (xcor-vect v1) (xcor-vect v2))
             (combiner (ycor-vect v1) (ycor-vect v2)))))

(define add-vect (comb-vectors +))
(define sub-vect (comb-vectors -))

(define (scale-vect scalar vector)
  (make-vect (* scalar (xcor-vect vector))
             (* scalar (ycor-vect vector))))

(display (make-vect 1 1))
(newline)
(display (add-vect (make-vect 1 1) (make-vect 2 2)))
(newline)
(display (sub-vect (make-vect 1 1) (make-vect 2 2)))
(newline)
(display (scale-vect 5.5 (make-vect 1 2)))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (cddr frame))

(define testframe (make-frame (make-vect -4 2) (make-vect -2 4) (make-vect 5 4)))
(newline)
(display testframe)
(newline)

(display (origin-frame testframe))
(newline)
(display (edge1-frame testframe))
(newline)
(display (edge2-frame testframe))

(define (make-segment v1 v2)
  (list v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cadr seg))

(define (for-each proc items)
  (cond ((null? items) #t)
        (else (proc (car items))
              (for-each proc (cdr items)))))

;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line-on-screen
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))

;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect
;     (origin-frame frame)
;     (add-vect (scale-vect (xcor-vect v)
;                           (edge1-frame frame))
;               (scale-vect (ycor-vect v)
;                           (edge2-frame frame))))))

;(segments->painter (list (make-segment (make-vect 1 1) (make-vect 2 2)) (make-segment (make-vect 3 4) (make-vect -3 4))))
;
;(define thismap (frame-coord-map (make-frame (make-vect -4 2) (make-vect -2 4) (make-vect 5 4))))
;(thismap (make-vect 1 1))


(define segment-items (list (make-segment 
                             (make-vect 0 0) 
                             (make-vect 0 .99))                                         
                            (make-segment 
                             (make-vect 0 0) (make-vect .99 0))                                         
                            (make-segment 
                             (make-vect 0 .99) (make-vect .99 .99))                                        
                            (make-segment 
                             (make-vect .99 0) (make-vect .99 .99))))

(define outline (segments->painter segment-items))
(newline)
(paint outline)

(define x-segments (list (make-segment
                          (make-vect 0 0)
                          (make-vect .99 .99))
                         (make-segment
                          (make-vect 0 .99)
                          (make-vect .99 0))))
(define x (segments->painter x-segments))
(newline)
(paint x)
                         
(define diamond-segments (list (make-segment
                                (make-vect .5 0)
                                (make-vect 1 .5))
                               (make-segment
                                (make-vect 1 .5)
                                (make-vect .5 1))
                               (make-segment
                                (make-vect .5 1)
                                (make-vect 0 .5))
                               (make-segment
                                (make-vect 0 .5)
                                (make-vect .5 0))))
(define diamond (segments->painter diamond-segments))
(newline)
(paint-hi-res diamond)

; 2.49 d.
; The wave painter.
(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))

(define wave (segments->painter wave-segments))
(paint wave)
(paint (flip-horiz wave))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(paint (beside wave))

;(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4))
;(display (outline (make-frame (make-vect -4 2) (make-vect -2 4) (make-vect 5 4))))