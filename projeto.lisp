;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Grupo 38 - Alameda - 72471 Michael Santos - 73245 Diogo Painho - 75219 Joao Franco ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load "utils.fas")

(defun ignore-value (x)
	(declare (ignore x))
	'ignore)

;;;;;;;;;;;;;;;
;;;; TOOLS ;;;;
;;;;;;;;;;;;;;;

(defun copia-array-2D (array1)
    (let((array2 (make-array (array-dimensions array1))))
        (dotimes(i (array-dimension array1 0))
            (dotimes(j (array-dimension array1 1))
                (setf (aref array2 i j) (aref array1 i j))))
        array2))

; @FIXME:
; quando descobre uma diferenca, coloca result a NIL e avalia o resto. Pouco
; eficiente porque tem que ir ate ao fim.
(defun iguais-array-2D(array1 array2)
    (let ((result t))
        (dotimes(i (array-dimension array1 0))
            (dotimes(j (array-dimension array1 1))
                (if (not (eq (aref array1 i j) (aref array2 i j)))
                    (setf result NIL))))
        result))

(defun equal-lists (lst1 lst2)
    (cond((and (null lst1) (null lst2))
        t)
    ((or (null lst1) (null lst2))
        NIL)
    (t
        (if (eq (car lst1) (car lst2))
            (equal-lists (cdr lst1) (cdr lst2))
            NIL))))


;;;;;;;;;;;;;;;;;;;;
;;;; TIPO ACCAO ;;;;
;;;;;;;;;;;;;;;;;;;;

;(defstruct (accao (:constructor cria-accao (coluna peca)))
;  coluna
;  peca)

(defun cria-accao (coluna peca)
  (cons coluna peca))

(defun accao-coluna (a)
  (car a))

(defun accao-peca (a)
  (cdr a))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO TABULEIRO ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tabuleiro
    linhas
    colunas
    array)

;NIL for empty and full otherwise
(defun cria-tabuleiro ()
    (let((lin 18) (col 10))
        (make-tabuleiro :linhas lin :colunas col :array (make-array (list lin col) :initial-element NIL))))

(defun copia-tabuleiro (tab1)
    (let((tab2 (copy-tabuleiro tab1)))
        (setf (tabuleiro-array tab2) (copia-array-2D (tabuleiro-array tab1)))
        tab2))

(defun tabuleiro-preenchido-p (tab lin col)
    (if (eq (aref (tabuleiro-array tab) lin col) NIL)
        NIL
        t))

(defun tabuleiro-altura-coluna (tab col)
    (do ((i (- (tabuleiro-linhas tab) 1) (- i 1)))
        ((or (= i -1) (tabuleiro-preenchido-p tab i col)) (+ i 1))
        ))

(defun tabuleiro-linha-completa-p (tab lin)
    (let ((i (- (tabuleiro-colunas tab) 1)))
		(loop
			(when (= i -1) (return t))
			(when (not (tabuleiro-preenchido-p tab lin i)) (return NIL))
			(setf i (- i 1)))))

(defun tabuleiro-preenche! (tab lin col)
    (cond ((or (>= lin (tabuleiro-linhas tab)) (>= col (tabuleiro-colunas tab))) '("error: tabuleiro-preenche"))
    (t
        (setf (aref (tabuleiro-array tab) lin col) t) )))

(defun tabuleiro-remove-linha! (tab lin)
    (do ((i lin (+ i 1)))
        ((= i (- (tabuleiro-linhas tab) 1)) NIL)
        (dotimes (j (tabuleiro-colunas tab))
            (setf (aref (tabuleiro-array tab) i j) (aref (tabuleiro-array tab) (+ i 1) j))))
    (dotimes (i (tabuleiro-colunas tab))
        (setf (aref (tabuleiro-array tab) (- (tabuleiro-linhas tab) 1) i) NIL)))

(defun tabuleiro-topo-preenchido-p (tab)
    (let ((i (- (tabuleiro-colunas tab) 1)))
		(loop
			(when (= i -1) (return NIL))
			(when (tabuleiro-preenchido-p tab (- (tabuleiro-linhas tab) 1) i) (return t))
			(setq i (- i 1)))))

(defun tabuleiros-iguais-p (tab1 tab2)
    (iguais-array-2D (tabuleiro-array tab1) (tabuleiro-array tab2)))

(defun tabuleiro->array (tab)
    (copia-array-2D (tabuleiro-array tab)))

(defun array->tabuleiro (array)
    (let ((tab (cria-tabuleiro)))
        (setf (tabuleiro-array tab) (copia-array-2D array))
        tab))

;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO ESTADO ;;;;
;;;;;;;;;;;;;;;;;;;;;

(defstruct estado
    pontos
    pecas-por-colocar
    pecas-colocadas ;lista ordenada da peca mas recente para a mais antiga
    tabuleiro)

(defun copia-estado (estado1)
    (let ((estado2 (copy-estado estado1)))
        (setf (estado-tabuleiro estado2) (copia-tabuleiro (estado-tabuleiro estado1)))
        (setf (estado-pecas-por-colocar estado2) (copy-list (estado-pecas-por-colocar estado1)))
        (setf (estado-pecas-por-colocar estado2) (copy-list (estado-pecas-por-colocar estado1)))
        estado2))

(defun estados-iguais-p (e1 e2)
    (cond((and (= (estado-pontos e1) (estado-pontos e2))
               (tabuleiros-iguais-p (estado-tabuleiro e1) (estado-tabuleiro e2))
               (equal-lists (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2))
               (equal-lists (estado-pecas-colocadas e1) (estado-pecas-colocadas e2)))
        t)
    (t
        NIL)))

(defun estado-final-p (estado)
    (if(or(tabuleiro-topo-preenchido-p (estado-tabuleiro estado))
          (null (estado-pecas-por-colocar estado))) t NIL))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO PROBLEMA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;
(defstruct problema
    estado-inicial
    solucao
    accoes
    resultado
    custo-caminho)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FUNCOES DO PROBLEMA DE PROCURA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; @See: Untested!
(defun solucao (estado)
    (if (and (null (estado-pecas-por-colocar estado)) (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))))
        t
        NIL))

; @See: How to make this a lambda
(defun accoes-aux (lst-accoes array-peca base-value)
    (dotimes (i (- base-value (array-dimension array-peca 1)))
        (setf lst-accoes (append lst-accoes (list (cria-accao i array-peca)))))
        ;(setf lst-accoes (cons (cria-accao i array-peca) lst-accoes))) @See: this would give us the list in reverse order. Would it be so much faster that reversing the list afterwards would conpensate? Dont think so
    lst-accoes)

; @See: Should we use with the cond or make it more generic? this way is faster no?
(defun accoes (estado)
    (let ((dotimes-value-base (+ (tabuleiro-colunas (estado-tabuleiro estado)) 1))
          (peca (first (estado-pecas-por-colocar estado)))
          (lst-accoes (list)))

        (cond ((eq peca 't) (setf lst-accoes (accoes-aux lst-accoes peca-t0 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-t1 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-t2 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-t3 dotimes-value-base)))
              ((eq peca 'l) (setf lst-accoes (accoes-aux lst-accoes peca-l0 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-l1 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-l2 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-l3 dotimes-value-base)))
              ((eq peca 'j) (setf lst-accoes (accoes-aux lst-accoes peca-j0 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-j1 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-j2 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-j3 dotimes-value-base)))
              ((eq peca 'i) (setf lst-accoes (accoes-aux lst-accoes peca-i0 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-i1 dotimes-value-base)))
              ((eq peca 's) (setf lst-accoes (accoes-aux lst-accoes peca-s0 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-s1 dotimes-value-base)))
              ((eq peca 'z) (setf lst-accoes (accoes-aux lst-accoes peca-z0 dotimes-value-base))
                            (setf lst-accoes (accoes-aux lst-accoes peca-z1 dotimes-value-base)))
              ((eq peca 'o) (setf lst-accoes (accoes-aux lst-accoes peca-o0 dotimes-value-base))))))


        ;#'(lambda(array-peca)
        ;    (dotimes (i (- (+ (tabuleiro-colunas (estado-tabuleiro estado)) 1) (array-dimension peca-t1 1)))
        ;        (setf lst-accoes (concatenate 'list lst-accoes (list (cria-accao i peca))))))))

(defun altura-inversa-peca (peca col)
    (do ((i 0 (incf i)))
        ((or (= i (array-dimension peca 1)) (eq (aref peca i col) T)) i)
    ))

(defun resultado (estado accao)
    (let ((e (copia-estado estado)))
        (setf (estado-pecas-colocadas e) (cons (first (estado-pecas-por-colocar e)) (estado-pecas-colocadas e))) ;atualiza pecas colocadas
        (setf (estado-pecas-por-colocar e) (rest (estado-pecas-por-colocar e))) ;atualiza pecas por colocar
        (let ((altura-temp 0) (primeira-linha 0))

            ;Calculate primeira-linha
            (dotimes (col (array-dimension (accao-peca accao) 1))
                (setf altura-temp (- (tabuleiro-altura-coluna (estado-tabuleiro e) (+ col (accao-coluna accao))) (altura-inversa-peca (accao-peca accao) col)))
                (cond ((> altura-temp primeira-linha)
                    (setf primeira-linha altura-temp))))

            ;(print primeira-linha)

            ;Preenche tabuleiro
            (dotimes (lin (array-dimension (accao-peca accao) 0))
                (dotimes (col (array-dimension (accao-peca accao) 1))
                    (cond ((eq T (aref (accao-peca accao) lin col))
                        (tabuleiro-preenche! (estado-tabuleiro e) (+ lin primeira-linha) (+ col (accao-coluna accao)))))))

            ;Testes
            ;verificar se o topo esta preenchido
            ;se sim nao se removem linhas e devolve-se o estado
            ;se nao removem-se as linhas e calculam-se os pontos
            (cond ((tabuleiro-topo-preenchido-p (estado-tabuleiro e))
                e)
            (t
                (let ((linhas-apagadas 0)
                      (limite (+ (array-dimension (accao-peca accao) 0) primeira-linha)))

                    (do ((lin limite (decf lin)))
                        ((= lin -1) t)
                        (cond ((tabuleiro-linha-completa-p (estado-tabuleiro e) lin)
                            (tabuleiro-remove-linha! (estado-tabuleiro e) lin)
                            (setf linhas-apagadas (incf linhas-apagadas)))))
                    (cond ((= linhas-apagadas 0)
                        t)
                    ((= linhas-apagadas 1) (write-line "1")(setf (estado-pontos e) (+ (estado-pontos e) 100)))
                    ((= linhas-apagadas 2) (write-line "2")(setf (estado-pontos e) (+ (estado-pontos e) 300)))
                    ((= linhas-apagadas 3) (write-line "3")(setf (estado-pontos e) (+ (estado-pontos e) 500)))
                    ((= linhas-apagadas 4) (write-line "4")(setf (estado-pontos e) (+ (estado-pontos e) 800)))
                    (t (write-line "*******************PreMIO HACkeR(impossivel remover mais que 5 linhas)****************"))) ))))


        e))

(defun qualidade (estado)
    (* (estado-pontos estado) -1))

;(defun custo-oportunidade (estado))

;;;;;;;;;;;;;;;;;
;;;; PROCURA ;;;;
;;;;;;;;;;;;;;;;;

;(defun procura-pp (problema))

;(defun procura-A* (problema heuristica))

;(defun procura-best (array lista-pecas))

;(setf e1 (make-estado :pontos 0 :pecas-por-colocar (list 'j 'o 'z 'i 'o 's 't 'l) :pecas-colocadas (list 'i) :tabuleiro (cria-tabuleiro)))
;(setf e1 (make-estado :pontos 0 :pecas-por-colocar (list 'i 'i 'i 'i 'i 'i 'i 'i 'i 'i 'i 'i 'i 'i ) :pecas-colocadas (list 'i) :tabuleiro (cria-tabuleiro)))
;(setf i 0)
;(loop
;	(when (estado-final-p e1) (return t))
;	(setf accoes (accoes e1))
;    (cond ((= (mod i 10) 0) (setf e1 (resultado e1 (first accoes))))
;          ((= (mod i 10) 1) (setf e1 (resultado e1 (first (cdr accoes)))))
;          ((= (mod i 10) 2) (setf e1 (resultado e1 (first (cdr (cdr accoes))))))
;          ((= (mod i 10) 3) (setf e1 (resultado e1 (first (cdr (cdr (cdr accoes)))))))
;          ((= (mod i 10) 4) (setf e1 (resultado e1 (first (cdr (cdr (cdr (cdr accoes))))))))
;          ((= (mod i 10) 5) (setf e1 (resultado e1 (first (cdr (cdr (cdr (cdr (cdr accoes)))))))))
;          ((= (mod i 10) 6) (setf e1 (resultado e1 (first (cdr (cdr (cdr (cdr (cdr (cdr accoes))))))))))
;          ((= (mod i 10) 7) (setf e1 (resultado e1 (first (cdr (cdr (cdr (cdr (cdr (cdr (cdr accoes)))))))))))
;          ((= (mod i 10) 8) (setf e1 (resultado e1 (first (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr accoes))))))))))))
;          ((= (mod i 10) 9) (setf e1 (resultado e1 (first (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr accoes))))))))))))))
;    (desenha-estado e1)
;	(setf i (incf i)))

;(print (qualidade e1))

;(desenha-estado e1)
;(setf accoes (accoes e1))
;(setf e2 (resultado e1 (first (cdr (cdr accoes)))))
;(setf accoes (accoes e2))
;(setf e3 (resultado e2 (first (cdr accoes))))
;(desenha-estado e2)
;(desenha-estado e3)
