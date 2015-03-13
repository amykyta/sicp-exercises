;2.72

; for most frequent it'll be O(1), each time it'll just check the left branch,
; find the element there and encode 0, then recurse and hit the base case.

; for least frequent it'll search n - 1 levels of the tree decrementing by 1 the number 
; of nodes at each one. At each level we have to search through all the
; elements to find the one we are looking for (it'll be last) so we will have ~O(n^2) growth