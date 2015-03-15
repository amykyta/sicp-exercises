;2.76
; * explicit dispatch on type inside each procedure
;  -	new type needs an update with a new clause to all the procedures to dispatch on it
;		and need to make sure no names clash
;  -	new operation is just a new generic procedure that dispatches on
; 		type to all the implementations

; * data-directed style 
;   - 	new type means adding a column to our column/procedure table
;   	needs a procedure imlementation and then a put operation to install imlementation
;   - 	new operation means adding a new row, so we need to update all our
;   	types that we have installed with this new procedure (putting it in the
;  		table for each type)

; * message-passing
;   - 	new type, means we just create a new procedure that returns a procedure
;   		to then dispatch to whatever data we need
;   -   new operation needs an update in each of these procedure objects we created above

; For a system where new types are being added we either want 
; data-directed or message-passing requiring least surgery

; For new operations it is better with the explicit dispatch or a properly
; factored data-directed system (make sure we group a package by operations?) 