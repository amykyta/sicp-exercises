#lang planet neil/sicp

; extended exercise on intervals in chapter 2

(define (make-interval a b) (cons a b))
(define (upper-bound x) (car x))
(define (lower-bound x) (cdr x))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

(define one-to-four (list 1 2 3 4))

(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(last-pair one-to-four)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (append-plus list1 list2)
  (cond ((null? list1) list2)
        ((not (pair? list1)) (cons list1 list2))
      ;  ((not (pair? list2)) (cons 
        (else (cons (car list1) (append-plus (cdr list1) list2)))))

;(define (reverse items)
;  (if (null? items)
;      
;      (cons (reverse (cdr items)) (cons (car items) nil))))

;(reverse one-to-four)


; i'm surprised this works, the branching in the elseif didnt seem like it
; would work, something about that recursion felt 'non-linear'
(define (same-parity . w)
  (define test (if (even? (car w)) even? odd?))
  (define (iter remaining-items)
    (if (null? remaining-items)
        nil
        (if (test (car remaining-items))
            (cons (car remaining-items)
                  (iter (cdr remaining-items)))
            (iter (cdr remaining-items)))))
  (iter w))



;(same-parity 1 2 3 4 5 6 7)
;(same-parity 2  3 4 5 6 7)

(define (square x)
  (* x x))

;(define (square-list items)
;  (if (null? items)
;      nil
;      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things) 
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (reverse-iter items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (car things) answer))))
  (iter items nil))



(reverse-iter (list 1 2 3 4 5 6))

(square-list (list 1 2 3 4 5))

(define (square-list-map items)
  (map square items))

(display (square-list-map (list 1 2 3 4 5)))

(define (for-each action items)
  (define (act items)
    (cond ((null? items) true)
          (else
            (action (car items))
            (act (cdr items)))))
  (act items))

(for-each (lambda (x) (newline) (display x)) (list 34 33 15 18 25))
        

;2.25
;(cadr (caddr (list 1 3 (list 5 7) 9)))
;(caar (list (list 7)))
;(cadadr (cadadr (cadadr (list 1 
;                              (list 2
;                                    (list 3
;                                          (list 4
;                                                (list 5
;                                                      (list 6 7)))))))))

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;(1 2 3 4 5 6)
(newline)
(display (cons x y))
(newline)
;((1 2 3) 4 5 6)
(display (list x y))
;((1 2 3) (4 5 6))

; problem #2.27
; modify the below procedure to do deep reverse
; just needed to check if what's being added to the reversed list
; is a pair or not, if it's a pair, reverse it too
;(define (reverse-iter items)
;  (define (iter things answer)
;    (if (null? things)
;        answer
;        (iter (cdr things)
;              (cons (car things) answer))))
;  (iter items nil))
(define (reverse-deep items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (if (pair? (car things))
                        (reverse-deep (car things))
                        (car things))
                    answer))))
  (iter items nil))

(newline)
(define xx (list (list 1 2) (list 3 4)))
(display xx)
(newline)
(display (reverse-deep xx))
(newline)
(display (reverse-iter xx))
(newline)


; 2.28
(define (fringe items)
; this was the solution by bill the lizard
; i tried this myself but got a bunch of nulls in the result
; because i didn't handle the null case
; this solution doesn't require a atomic value handling append-plus
;    (cond ((null? items) nil)
;        ((not (pair? items)) (list items))
;        (else (append 
;               (fringe (car items)) 
;               (fringe (cdr items))))))
  (if (not (pair? items)) 
      items
      (append-plus (fringe (car items)) (fringe (cdr items)))))

(define xxx (list (list 1 2) (list 3 4)))

(display (fringe (list xxx xxx)))

; 2.29

(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
;  (display "branch")
;  (newline)
;  (display branch)
;  (newline)
  (car (cdr branch)))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (number? struct) 
        struct
        (+ (branch-weight (left-branch struct))
           (branch-weight (right-branch struct))))))


(define (total-weight mobile)
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (+ (branch-weight left) (branch-weight right))))
;    (let ((structure-left (branch-structure left))
;          (structure-right (branch-structure right)))
;      (+ (if (number? structure-left) structure-left (total-weight structure-left))
;         (if (number? structure-right) structure-right (total-weight structure-right))))))

(define b (make-mobile 
               (make-branch 2 
                            (make-mobile 
                             (make-branch 2 12)
                             (make-branch 2 9)))
               (make-branch 2 10)))
(newline)
(total-weight b)

; 2.29 part (c) im amazed this works...
(define (balanced? mobile)
  (let ((torque (lambda (branch) 
                  (* (branch-weight branch) (branch-length branch)))))
    (cond ((= (torque (left-branch mobile))
              (torque (right-branch mobile)))
           (let ((left-b-pair? (pair? (branch-structure (left-branch mobile))))
                 (right-b-pair? (pair? (branch-structure (right-branch mobile)))))
             (cond ((and (not left-b-pair?) (not right-b-pair?))
                    #t)
                   ((and left-b-pair? right-b-pair?)
                    (and (balanced? (branch-structure (left-branch mobile)))
                         (balanced? (branch-structure (right-branch mobile)))))
                   (left-b-pair?
                    (balanced? (branch-structure (left-branch mobile))))
                   (right-b-pair?
                    (balanced? (branch-structure (right-branch mobile)))))))    
          (else #f))))

(balanced? b)

; 2.29 part (d)
; very little..just the corresponding selectors replacing
; (car (cdr x)) with just (cdr x)

(define (square-tree tree)
;  (cond ((null? tree) nil)
;        ((not (pair? tree)) (square tree))
;        (else (cons (square-tree (car tree))
;                    (square-tree (cdr tree))))))
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square-map-tree tree)
  (tree-map square tree))

(display (square-tree
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7))))
(newline)

(display (square-map-tree
          (list 1
                (list 2 (list 3 4) 5)
                (list 6 7))))

; 2.32 - the moneyshot is the proc passed into the map
; which takes a list and appends to the front the head of the
; current list. So it creates subsets from all elements but the
; current one and then adds the current one to them.
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (tree)
                            (cons (car s) tree))
                          rest)))))

(newline)
(display (subsets (list 1 2 3)))


(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq) (accumulate op initial (cdr seq)))))

;2.33
(define (map-1 p sequence)
  (accumulate (lambda (x y) (cons (square x) y)) nil sequence))
(newline)
(display (map-1 square (list 1 2 3 4 5)))

(define (append-1 seq1 seq2)
  (accumulate cons seq2 seq1))
(newline)
(display (append-1 (list 1 2 3) (list 4 5 6)))

(define (length-1 sequence)
  (accumulate (lambda (_ y) (inc y)) 0 sequence))
(newline)
(display (length-1 (list 1 2 3 4 5 6)))

; 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))
(newline)
;mind=blown
(display (horner-eval 2 (list 1 3 0 5 0 1)))

; 2.35
; mind blown again (counting the leaves from the bottom up then summing across the top nodes)
; at each node, either count it as 1 leaf, or if it's a node not a leaf
; recurse and count its leaves. So we end up with a list of top level nodes
; and their count for number of leaves. This is where accumulate comes in
; to sum them all up
(define (count-leaves-1 t)
  (accumulate (lambda (x y) (+ x y)) 0 
              (map (lambda (sub-tree)
                     (cond ((null? sub-tree) 0)
                           ((not (pair? sub-tree)) 1)
                           (else (count-leaves-1 sub-tree))))
                   t)))
(newline)
(display (count-leaves-1 (list 1 (list 4 (list 9 16) 25) (list 36 49))))

;2.36
; create a list of cars and accumulate it
; create a list of cdrs in lists and recurse
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (l) (car l)) seqs))
            (accumulate-n op init (map (lambda (l) (cdr l)) seqs)))))
(newline)
(display (accumulate-n + 0 (list  
                            (list 1 2 3) 
                            (list 4 5 6) 
                            (list 7 8 9) 
                            (list 10 11 12))))

;2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w))) ; this is the scheme map which takes more than one list and applies op 'vertically' to elemnts and returns a results list
(newline)
(display "dot-product") (newline)
(display (dot-product (list 1 2 3) (list 3 4 5)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))
(newline)
(display (list 1 2 3))
(newline)
(display (list 4 5 6))
(newline)
(display (list 7 8 9))
(newline)
(display "times")
(newline)
(display (list 1 2 3))
(newline)
(display "= ")
(define m (list (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9)))
(display (matrix-*-vector m (list 1 2 3)))

; freaking beautiful
(define (transpose mat)
  (accumulate-n cons nil mat))
(newline)
(display m) (newline)
(display "transposed")(newline)
(display (transpose m))


;i don't know..matrix multiplication
; dot-product rows of m w/ columns of n, and move down row x [columns]
; first then down the rows
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) 
           (map (lambda (y) (dot-product x y))
                cols))
           m)))

;bill's solution (each row of resulting matrix is the product
;of the row in the first matrix * the second matrix
;(define (matrix-*-matrix m n)
;  (let ((cols (transpose n)))
;    (map (lambda (row) (matrix-*-vector cols row)) m)))

(newline)
(display (matrix-*-matrix m m))

;2.38
; commutativity? and associativity too apparently

(define fold-right accumulate)
;2.39
(define (reverse-right seq)
  (fold-right (lambda (x y)
                (append y (list x)))
              nil
              seq))
   
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define (reverse-left seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))
(newline)
(display (list 1 2 3 4 5)) (newline)
(display "reversed..") (newline)
(display (reverse-right (list 1 2 3 4 5)))
