;2.7
(define rock-dictionary '((NA 16) (YIP 9) (SHA 3) (A 2) (GET 2) (JOB 2) (BOOM 1) (WAH 1)))

(define song '(GET A JOB SHA na na na na na na na na GET A JOB SHA na na na na na na na na WAH yip yip yip yip yip yip yip yip yip SHA BOOM))

; encoded with this many bits
(length (encode song (car (generate-huffman-tree rock-dictionary))))
; with fixed encoding it would be this
(* 3 (length song))