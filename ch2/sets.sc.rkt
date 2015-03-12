#lang planet neil/sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))

;2.60
; Any place where most of the operations are union and element-set? since those
; will perform well. Not really sure what those applications are. 

;2.61
(define (element-of-ordered-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-ordered-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoin-ordered-set x (cdr set))))))
;2.62
(define (union-ordered-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (union-ordered-set (cdr set1) set2))
                      ((< x1 x2)
                       (cons x1 (union-ordered-set (cdr set1) set2)))
                      ((> x1 x2)
                       (cons x2 (union-ordered-set set1 (cdr set2)))))))))


(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


; 2.63 - yes, the same list each one. The first one has O(n^2) growth because of the appending, second one should be O(n)?
; or do both have O(logn) because they split at level of the tree?

; list to tree 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; 2.64
; a) Splits the tree into 2 by getting the quotient of n-1 list length / 2
; then constructs a left branch from the first (n-1)/2 elements
; then creates a top node from the first element of the remaining ones and a right
; branch from the (cdr remaining-ones) (right side of the list) of length
; that is 1 less than the size of the whole list - left branch
; finally it returns a pair of tree + remaining elements
; but when will there ever be remaining ones? I can't see it...
; i've run through examples of 2, 3, 5, 6, 7 in my head and don't see remaining ones
; actually, i think it only is ever needed when the number of elements is more
; than the size of the tree requested
; b) O(logn)? since it halves the at each step? 
;  ** really? O(n) since it visits each node once? same for 2.63.. (except the one
; with append apparently is O(logn) because append halves at each step...but
; isn't append implemented in terms of consing each damn thing onto the second list?
; or i guess we don't worry about implementaion



(define (element-of-set-tree? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set-tree? x (left-branch set)))
        ((> x (entry set))
         (element-of-set-tree? x (right-branch set)))))

(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set) 
                    (adjoin-set-tree x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-tree x (right-branch set))))))

; 2.65 - we can just tree->list x union or interesect ->into balanced tree again


;2.66

(define (make-record key data)
  (cons key data))

(define (key record)
  (car record))

(define (data record)
  (cdr record))

(define yogurts-database
  (list->tree (list (make-record 1 'Coconut)
        (make-record 2 'Peach)
        (make-record 3 'Blueberry)
        (make-record 4 'Mixed-berry))))

(define (lookup given-key set-of-records)
  (if (null? set-of-records) '()
      (let ((current-key (key (entry set-of-records)))
            (current-record (entry set-of-records)))
        (cond  ((equal? given-key current-key) current-record)
               ((< given-key current-key)
                (lookup given-key (left-branch set-of-records)))
               ((> given-key current-key)
                (lookup given-key (right-branch set-of-records)))))))

