;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Grupo 38 - Alameda - 72471 Michael Santos - 73245 Diogo Painho - 75219 Joao Franco ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load utils.lisp)

;;;;;;;;;;;;;;;;;;;;
;;;; TOOLS ;;;;
;;;;;;;;;;;;;;;;;;;;

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

(defstruct accao
  coluna
  peca)

(defun cria-accao (inteiro array)
  (make-accao :coluna inteiro :peca array))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO TABULEIRO ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct tabuleiro
    linhas
    colunas
    array)

;NIL for empty and full otherwise
(defun cria-tabuleiro ()
    (let((lin 10) (col 18))
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
    if (and (null (estado-pecas-por-colocar estado)) (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))))
        t
        NIL)

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
        

(defun resultado (estado accao)
    (let ((e (copia-estado estado)))
        (setf (estado-pecas-colocadas e) (cons (first (estado-pecas-por-colocar e)) (estado-pecas-colocadas e))) ;atualiza pecas colocadas
        (setf (estado-pecas-por-colocar e) (rest (estado-pecas-por-colocar e))) ;atualiza pecas por colocar
        (let ((altura-temp 0) (primeira-linha 0))
            
            ;Calculate primeira-linha ;@FIXME: pecas nao encaixam
            (dotimes (col (array-dimension (accao-peca accao) 1))
                (setf altura-temp (tabuleiro-altura-coluna (estado-tabuleiro e) (+ col (accao-coluna accao))))
                (cond ((> altura-temp primeira-linha)
                    (setf primeira-linha altura-temp))))
            
            (print primeira-linha)
            ;Preenche tabuleiro
            (dotimes (lin (array-dimension (accao-peca accao) 0))
                (dotimes (col (array-dimension (accao-peca accao) 1))
                    (cond ((eq T (aref (accao-peca accao) lin col))
                        (tabuleiro-preenche! (estado-tabuleiro e) (+ lin primeira-linha) (+ col (accao-coluna accao))))))))
            
            ;@TODO: Testes
        e))

;(defun qualidade (estado))

;(defun custo-oportunidade (estado))

;;;;;;;;;;;;;;;;;
;;;; PROCURA ;;;;
;;;;;;;;;;;;;;;;;

;(defun procura-pp (problema))

;(defun procura-A* (problema heuristica))

;(defun procura-best (array lista-pecas))

(setf e1 (make-estado :pontos 0 :pecas-por-colocar (list 'o 'o 'z) :pecas-colocadas (list 'i) :tabuleiro (cria-tabuleiro)))
(print e1)
(setf accoes (accoes e1))
(setf e2 (resultado e1 (first (cdr (cdr accoes)))))
(setf e3 (resultado e2 (first (cdr (cdr accoes)))))
(print e2)
(print e3)
;(print accoes)


;(concatenate 'list '(1 2 3) '(4 5))