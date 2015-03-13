(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

;2.69

; this wasn't needed after all, the adjoin-set above is sufficient and 
; this is a bad reimplementation of it
(define (insert-new-node new-node node-list)
  (cond ((null? node-list) (list new-node))
        ((< (weight new-node) (weight (car node-list)))
         (cons new-node node-list))
        ((> (weight new-node) (weight (car node-list)))
         (cons (car node-list) (insert-new-node new-node (cdr node-list))))
        (else (cons new-node node-list))))

(define (successive-merge leaf-set-pairs)
  (if (null? (cdr leaf-set-pairs))
      leaf-set-pairs
      (successive-merge (insert-new-node
                         (make-code-tree (car leaf-set-pairs)
                                         (cadr leaf-set-pairs))
                         (cddr leaf-set-pairs)))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))