;; Test 1
(setf q (make-binary-heap))

(min-heap-insert q 'C1 3)
(min-heap-insert q 'C2 3)
(min-heap-insert q 'B2 2)
(min-heap-insert q 'B1 2)
(min-heap-insert q 'A1 1)
(min-heap-insert q 'A2 1)
(min-heap-insert q 'A3 1)


(loop
    (setf a (heap-pop q))
	(when (null a) 
		(return NIL))
    (print a))

(print q)