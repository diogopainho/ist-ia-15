;;; BINARY HEAP ;;;

(declaim (inline heap-parent))
(defun heap-parent (i)
	(floor i 2)) ; i/2

(declaim (inline heap-left))
(defun heap-left (i)
	(* 2 i)) ;2i

(declaim (inline heap-right))
(defun heap-right (i)
	(+ (* i 2) 1)) ;2i + 1

(defstruct node
	element
	key)

;@See: make sure array starts in the index 1
(defun make-binary-heap (&optional (default-increment 512))
	(make-array default-increment :fill-pointer 1 :adjustable t))

(defun heap-last-pos (A)
	(- (fill-pointer A) 1))

(defun min-heapify (A i)
	(let ((l (heap-left i))
		  (r (heap-right i))
		  smallest)
		
		;Testa se a folha esquerda tem uma key mais pequena que o i (parent). Faz set do smallest de acordo.
		(if (and (<= l (heap-last-pos A)) (< (node-key (aref A l)) (node-key (aref A i))))
			(setf smallest l) ;true
			(setf smallest i)) ;false
		
		;Testa se a folha direita tem uma key. Faz set do smallest de acordo e ficamos com o menor dos 3.
		(when (and (<= r (heap-last-pos A)) (< (node-key (aref A r)) (node-key (aref A smallest))))
			(setf smallest r))

		;Caso exista um menor que o i (parent), entao trocamos e fazes heapify na arvore abaixo.
		(when (/= smallest i)
			(rotatef (aref A i) (aref A smallest)) ;swap
			(min-heapify A smallest))))

(defun heap-pop (A)
	;Testa tentar remover de uma heap sem elementos (underflow)
	(when (< (heap-last-pos A) 1) 
		(write-line "head-pop: Underflow")
		(return-from heap-extract-min NIL))

	;Gurda 'min' o valor na primeira casa que deve ser retornado
	;@See: possivelmente precisamos de fazer uma hard-copia do node
	(let ((min (aref A 1)))
		(setf (aref A 1) (aref A (heap-last-pos A)))
		(decf (fill-pointer A))
		(min-heapify A 1)
		min))

(defun heap-decrease-key (A i key)
	;Testa se a key recebida e maior que a key original de i na heap
	(when (> key (node-key (aref A i)))
		(write-line "heap-decrease-key: Key received not smaller than the original key.")
		(return-from heap-decrease-key NIL))

	;Atualiza o valor da key na posicao i
	(setf (node-key (aref A i)) key)

	;Atualiza a heap para ficar consistente
	(loop while (and (> i 1) (> (node-key (aref A (heap-parent i))) (node-key (aref A i)))) do
		(rotatef (aref A (heap-parent i)) (aref A i)) ;swap
		(setf i (heap-parent i))))

(defconstant infinite-positive-number 2147483647)
(defun min-heap-insert (A element key)
	(vector-push-extend (make-node :element element :key infinite-positive-number) A) ;Test with increment to check performance
	(heap-decrease-key A (heap-last-pos A) key))

;(setf q (make-binary-heap))
;(print (heap-last-pos q))
;(min-heap-insert q 'C 3)
;(min-heap-insert q 'B 2)
;(min-heap-insert q 'A 1)

;(print (heap-pop q))

;(print q)
