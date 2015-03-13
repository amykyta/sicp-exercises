;2.68

(define (in-branch? symbol symbol-list)
  (cond ((null? symbol-list) #f)
        ((eq? symbol (car symbol-list)) #t)
        (else (in-branch? symbol (cdr symbol-list)))))
      
(define (encode-symbol symbol tree)
  (if  (leaf? tree)
       '()
       (let ((bit (if (in-branch? symbol (symbols (right-branch tree))) 
                      1
                      (if (in-branch? symbol (symbols (left-branch tree)))
                          0
                          (error "bad symbol - not part of the tree" symbol)))))
         (let ((remaining-tree (if (= bit 0) 
                                   (left-branch tree)
                                   (right-branch tree))))
           
           (cons bit (encode-symbol remaining-tree))))))

; someone's nice solution
; (define (encode-symbol symbol tree) 
;   (if (not (element-of-set? symbol (symbols tree))) 
;       (error "symbol cannot be encoded") 
;       (if (leaf? tree) 
;           '() 
;           (let ((left-set (symbols (left-branch tree))) 
;                 (right-set (symbols (right-branch tree)))) 
;             (cond ((element-of-set? symbol left-set) 
;                    (cons 0 (encode-symbol symbol (left-branch tree)))) 
;                   ((element-of-set? symbol right-set) 
;                    (cons 1 (encode-symbol symbol (right-branch tree)))))))))
  
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))