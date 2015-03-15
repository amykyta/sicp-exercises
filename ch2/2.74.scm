;2.74
;a) 
(define (get-record employee-id file)
	((apply-generic 'get-record file) employee-id))
; the files need to carry a type-tag so that a table of 
; division x get-procedure can be built

;b) 
(define (get-salary employee-record)
	(apply-generic 'get-salary employee-record))
; so long as each employee-record carries a division tag, we should
; be able to pick out a get-salary procedure that knows how to get
; salary info from it ('north-east record)

;c)
(define (get-employee-record name files)
	(if (null? files) #f)

	(let (record (get-record name (car file)))
		(if (record)
			record
			(get-employee-record name (cdr files)))
		))

;d) 
; Should be just a matter of adding a new column for the division
; along with implementation for all the get/set procedures

