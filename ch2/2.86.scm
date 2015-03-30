; 2.86
;
; replace all +, -, *, /, =, and sin, cos, atan procedures in the complex package 
; with generic ones that are installed, adding
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x) (apply-generic 'arctan x))

; to existing add, sub, mul, div, and equ?
; 
; Ideally this would have been all we have to do but because our current
; implementation of apply-generic first tries to drop all arguments to 
; the lowest type they can be, and then when a procedure isn't found for
; a combination of types it tries to raise the lower one we end up in a loop.
; 
; Need to think more about how to deal with this.
