;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Grupo 38 - Alameda - 72471 Michael Santos - 73245 Diogo Painho - 75219 Joao Franco ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;
;;;; TOOLS ;;;;
;;;;;;;;;;;;;;;

;Funcao auxiliar que dado um array devolve outro array igual, com referencias independentes
(defun copia-array-2D (array1)
    (let((array2 (make-array (array-dimensions array1))))
        (dotimes(i (array-dimension array1 0))
            (dotimes(j (array-dimension array1 1))
                (setf (aref array2 i j) (aref array1 i j))))
        array2))

; @FIXME:
; quando descobre uma diferenca, coloca result a NIL e avalia o resto. Pouco
; eficiente porque tem que ir ate ao fim.
;Funcao auxiliar que dados dois arrays, verifica se estes sao iguais
(defun iguais-array-2D(array1 array2)
    (let ((result t))
        (dotimes(i (array-dimension array1 0))
            (dotimes(j (array-dimension array1 1))
                (if (not (eq (aref array1 i j) (aref array2 i j)))
                    (setf result NIL))))
        result))

;Funcao auxiliar que dadas duas listas, verifica se estas sao iguais
(defun equal-lists (lst1 lst2)
    (cond((and (null lst1) (null lst2))
        t)
    ((or (null lst1) (null lst2))
        NIL)
    (t
        (if (eq (car lst1) (car lst2))
            (equal-lists (cdr lst1) (cdr lst2))
            NIL))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO PRIORITY QUEUE ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defstruct priority-queue
    (size 0)
    (l NIL))

(defun p-queue-empty (pq)
    (if (zerop (priority-queue-size pq))
        T
        NIL))

;@Robustness: should test if queue is empty before poping?
(defun p-queue-pop (pq)
    ;(p-queue-print-values pq "in pop")
    (decf (priority-queue-size pq))
    (pop (priority-queue-l pq)))

(defun p-queue-insert (pq newEle newVal)
    ;(p-queue-print-values pq "in insert")
    (cond ((p-queue-empty pq)
        (incf (priority-queue-size pq))
        (setf (priority-queue-l pq) (list (list newEle newVal))))
    (T
        (let ((i 0))
            (loop
                (when (>= i (priority-queue-size pq)) (return))
                (when (>= (second (nth i (priority-queue-l pq))) newVal) (return))
                (incf i))
            (incf (priority-queue-size pq))
            
            (cond ((eq i 0)
                (setf (priority-queue-l pq) (cons (list newEle newVal) (priority-queue-l pq))))
            (T
                (push (list newEle newVal) (cdr (nthcdr (1- i) (priority-queue-l pq))))))))))

              
(defun p-queue-print-values (pq &optional (extraText ""))
    (write extraText)
    (write "priotity-queue values: (")
    (dolist (ele (priority-queue-l pq))
        (format T "~A, " (second ele)))
	(write-line ")")
	(write-line "")
    (write-line ""))
    
    
;;;;;;;;;;;;;;;;;;;;
;;;; TIPO ACCAO ;;;;
;;;;;;;;;;;;;;;;;;;;
;Construtor que dado um inteiro e uma peca, devolve um par com os dois elementos designado por accao
(defun cria-accao (coluna peca)
  (cons coluna peca))

;Selector que dada uma accao devolve a coluna mais a esquerda a partir da qual a peca vai ser colocada
(defun accao-coluna (a)
  (car a))

;Selector que dada uma accao devolve o array com a configuracao geometrica exacta com que vai ser colocada
(defun accao-peca (a)
  (cdr a))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO TABULEIRO ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;Estrutura que define o tabuleiro como tendo linhas colunas e um array
(defstruct tabuleiro
    linhas
    colunas
    array)

;Construtor que devolve um tabuleiro vazio
(defun cria-tabuleiro ()
    (let((lin 18) (col 10))
        (make-tabuleiro :linhas lin :colunas col :array (make-array (list lin col) :initial-element NIL))))

;Constrtor que recebe um tabuleiro e devolve um novo tabuleiro com o mesmo conteudo do tabuleiro recebido. E garantido que qualquer alteracao nao se propaga para o tabuleiro original
(defun copia-tabuleiro (tab1)
    (let((tab2 (copy-tabuleiro tab1)))
        (setf (tabuleiro-array tab2) (copia-array-2D (tabuleiro-array tab1)))
        tab2))

;Seletor que recebe um tabuleiro, um inteiro correspondente a linha e coluna e devolve o valor logico caso essa posicao esteja preenchida ou nao
(defun tabuleiro-preenchido-p (tab lin col)
    (if (eq (aref (tabuleiro-array tab) lin col) NIL)
        NIL
        t))

;Selector que recebe um tabuleiro e uma coluna e devolve a altura da uma coluna, ou seja a posicao mais alta que esta preenchida nessa coluna
(defun tabuleiro-altura-coluna (tab col)
    (do ((i (- (tabuleiro-linhas tab) 1) (- i 1)))
        ((or (= i -1) (tabuleiro-preenchido-p tab i col)) (+ i 1))
        ))

;Reconhecedor que recebe um tabuleiro e um inteiro correspondente a uma linha e devolve o valor logico com base nas posicoes da linha que estiverem preenchidas
(defun tabuleiro-linha-completa-p (tab lin)
    (let ((i (- (tabuleiro-colunas tab) 1)))
		(loop
			(when (= i -1) (return t))
			(when (not (tabuleiro-preenchido-p tab lin i)) (return NIL))
			(setf i (- i 1)))))

;Modificador que recebe um tabuleiro, uma linha e uma coluna e preenche a posicao dada no tabuleiro
(defun tabuleiro-preenche! (tab lin col)
    (cond ((or (>= lin (tabuleiro-linhas tab)) (>= col (tabuleiro-colunas tab))) '("error: tabuleiro-preenche"))
    (t
        (setf (aref (tabuleiro-array tab) lin col) t) )))

;Modificador que recebe um tabuleiro e uma linha e altera o tabuleiro removendo a linha correspondente. As linhas que estao por baixo nao sao alteradas
(defun tabuleiro-remove-linha! (tab lin)
    (do ((i lin (+ i 1)))
        ((= i (- (tabuleiro-linhas tab) 1)) NIL)
        (dotimes (j (tabuleiro-colunas tab))
            (setf (aref (tabuleiro-array tab) i j) (aref (tabuleiro-array tab) (+ i 1) j))))
    (dotimes (i (tabuleiro-colunas tab))
        (setf (aref (tabuleiro-array tab) (- (tabuleiro-linhas tab) 1) i) NIL)))

;Reconhecedor que recebe um tabuleiro e devolve o valor logico T se todas as posicoes do topo do tabuleiro estiverem preenchidas e NIL caso contrario
(defun tabuleiro-topo-preenchido-p (tab)
    (let ((i (- (tabuleiro-colunas tab) 1)))
		(loop
			(when (= i -1) (return NIL))
			(when (tabuleiro-preenchido-p tab (- (tabuleiro-linhas tab) 1) i) (return t))
			(setq i (- i 1)))))

;Teste que receve dois tabuleiros e devolve o valor logico T caso sejam iguais e NIL caso contrario
(defun tabuleiros-iguais-p (tab1 tab2)
    (iguais-array-2D (tabuleiro-array tab1) (tabuleiro-array tab2)))

;Transformador de saida que recebe um tabuleiro e devolve um novo array com 16 linhas e 10 colunas em que cada linha e coluna deverar contar o valor logico correspondente a cada posicao do tabuleiro
(defun tabuleiro->array (tab)
    (copia-array-2D (tabuleiro-array tab)))

;Transformador de entrada que recebe um array de 16 linhas e 10 colunas cujas posicoes tem de ter o mesmo valor logico T ou NIL e constroi um novo tabuleiro com o conteudo do array recebido
(defun array->tabuleiro (array)
    (let ((tab (cria-tabuleiro)))
        (setf (tabuleiro-array tab) (copia-array-2D array))
        tab))

;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO ESTADO ;;;;
;;;;;;;;;;;;;;;;;;;;;

;Estrutura que define o estado com tendo pontos, pecas por colocar, pecas colocadas e um tabuleiro
(defstruct estado
    (pontos 0)
    pecas-por-colocar
    pecas-colocadas ;lista ordenada da peca mas recente para a mais antiga
    tabuleiro)

;Construtor que recebe um estado e devolve um estado cujo conteudo deve ser copiado a partir do estado original
(defun copia-estado (estado1)
    (let ((estado2 (copy-estado estado1)))
        (setf (estado-tabuleiro estado2) (copia-tabuleiro (estado-tabuleiro estado1)))
        (setf (estado-pecas-por-colocar estado2) (copy-list (estado-pecas-por-colocar estado1)))
        (setf (estado-pecas-colocadas estado2) (copy-list (estado-pecas-colocadas estado1)))
        estado2))

;Teste que recebe dois estados e devolve o valor logico T caso os estados sejam iguais ou NIL caso constrario
(defun estados-iguais-p (e1 e2)
    (cond((and (= (estado-pontos e1) (estado-pontos e2))
               (tabuleiros-iguais-p (estado-tabuleiro e1) (estado-tabuleiro e2))
               (equal-lists (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2))
               (equal-lists (estado-pecas-colocadas e1) (estado-pecas-colocadas e2)))
        t)
    (t
        NIL)))

;Reconhecedor que dado um estado, devolve o valor logico T se corresponder a um estado final onde o jogador ja nao pode fazer mais jogadas e falso caso contrario
;Um estado e considerado final se o tabuleiro nao tiver atingido o topo ou se ja nao existirem pecas por colocar
(defun estado-final-p (estado)
    (if(or(tabuleiro-topo-preenchido-p (estado-tabuleiro estado))
          (null (estado-pecas-por-colocar estado))) t NIL))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO PROBLEMA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;Estrutura que define o problema como tendo estado-inicial, solucao, accoes, resultado e o custo-caminho
(defstruct problema
    estado-inicial
    solucao
    accoes
    resultado
    custo-caminho)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FUNCOES DO PROBLEMA DE PROCURA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Funcao que recebe um estado e devolve o valor logico verdade se o estado recebido corresponder a uma solucao e falso caso constrario
;Um estado do jogo Tetris e considerado colucao se o topo do tabuleiro nao estiver preenchido e se ja nao existirem pecas por colocar
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

;Funcao que recebe um estado e devolve uma lista de accoes correspondendo a todas as accoes validas que podem ser feitas com a proxima pec a ser colocada
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

;
(defun altura-inversa-peca (peca col)
    (do ((i 0 (incf i)))
        ((or (= i (array-dimension peca 1)) (eq (aref peca i col) T)) i)
    ))

;Funcao que recebe um estado e uma accao e devolve um novo estado que resulta de aplicar a accao recebida no estado original
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
                    ((= linhas-apagadas 1) (setf (estado-pontos e) (+ (estado-pontos e) 100)))
                    ((= linhas-apagadas 2) (setf (estado-pontos e) (+ (estado-pontos e) 300)))
                    ((= linhas-apagadas 3) (setf (estado-pontos e) (+ (estado-pontos e) 500)))
                    ((= linhas-apagadas 4) (setf (estado-pontos e) (+ (estado-pontos e) 800)))
                    (t (write-line "*******************PreMIO HACkeR(impossivel remover mais que 5 linhas)****************"))) ))))


        e))

;Funcao que recebe um estado e retorna um valor de qualidade que corresponde ao valor negativo dos pontos ganhos ate ao momento
(defun qualidade (estado)
    (* (estado-pontos estado) -1))

;Funcao que dado um estado devolve o custo de oportunidade de todas as accoes realizadas ate ao momento, assumindo que e sempre possivel fazer o maximo de pontos por cada peca colocada
(defun custo-oportunidade (estado)
    (let ((maxPontos 0))
        (dolist (n (estado-pecas-colocadas estado))
            (cond ((eq n 'i) (setf maxPontos (+ maxPontos 800)))
                ((eq n 'j) (setf maxPontos (+ maxPontos 500)))
                ((eq n 'l) (setf maxPontos (+ maxPontos 500)))
                ((eq n 's) (setf maxPontos (+ maxPontos 300)))
                ((eq n 'z) (setf maxPontos (+ maxPontos 300)))
                ((eq n 't) (setf maxPontos (+ maxPontos 300)))
                ((eq n 'o) (setf maxPontos (+ maxPontos 300)))))
        (- maxPontos (estado-pontos estado))))

;;;;;;;;;;;;;;;;;
;;;; PROCURA ;;;;
;;;;;;;;;;;;;;;;;
;@Test: Preencher tabuleiro todo e testar a ver se retorna nil
(defun procura-pp (p)
    (recursive-pp p))

(defun recursive-pp (p)
    (cdr (recursive-pp-aux p (problema-estado-inicial p) NIL)))

(defun recursive-pp-aux (p e oldA)
    (cond ((eq (funcall (problema-solucao p) e) T)
        (list oldA))
    (T
        (let ((result NIL))
            (dolist (a (funcall (problema-accoes p) e))
                (setf result (recursive-pp-aux p (funcall (problema-resultado p) e a) a))
                (when (not (eq result NIL))
                    (return-from recursive-pp-aux (cons oldA result)))))
        NIL)))

;(defstruct problema
;    estado-inicial // e o estado inical
;    solucao        // funcao que diz se o estado que recebe e uma solucao
;    accoes         // recebe um estado e devolve uma lista de accoes
;    resultado      // aplica uma accao num estado e devolve o novo estado
;    custo-caminho) // devolve um valor que quanto mais baixo melhor (corresponde a qualidade do estado)

;Retorna a diferenca entre o max e o avg nivel das pecas caso o max seja maior que 4. Caso contrario devolve 0.
(defun average-height-h (e)
    (let ((currColH NIL) 
	      (maxH 0) 
		  (average 0))
        (dotimes (i (tabuleiro-colunas (estado-tabuleiro e)))
            (setf currColH (tabuleiro-altura-coluna (estado-tabuleiro e) i))
            (when (> currColH maxH) (setf maxH currColH))
			(setf average (+ average currColH)))
		(setf average (round average (tabuleiro-colunas (estado-tabuleiro e))))
		;(format T "average: ~A" average)
		;(write-line "")
		;(format T "max: ~A" maxH)
		;(write-line "")
        ;(if (> maxH 4)
		;	(- maxH average)
		;	0)))
		(- maxH average)))

(defun tabuleiro-buracos-coluna (tab col)
	 (let ((holeAmount 0)
		   (foundEmpty NIL))
		(dotimes (i (tabuleiro-linhas tab))
			(cond ((and (eq foundEmpty NIL) (eq (tabuleiro-preenchido-p tab i col) NIL)) ;Se nunca encontrou uma casa vazia e agora encontrou
				(setf foundEmpty T))
			((and (eq foundEmpty T) (eq (tabuleiro-preenchido-p tab i col) T)) ;Se encontrou empty e agora a casa esta preenchida entao temos um buraco
				(incf holeAmount)
				(setf foundEmpty NIL))))
		holeAmount))

(defun holes-h (e)
    (let ((holeAmount 0))
        (dotimes (i (tabuleiro-colunas (estado-tabuleiro e)))
			(setf holeAmount (+ holeAmount (tabuleiro-buracos-coluna (estado-tabuleiro e) i))))
		holeAmount))
	


(defun best-first-search (p F)
    (let ((node NIL) 
          (frontier (make-priority-queue))
          (nodesExpanded 0)
		  (nodesGenerated 0))
        (p-queue-insert frontier (problema-estado-inicial p) (funcall F (problema-estado-inicial p)))
        (loop
            (incf nodesExpanded)

            (if (p-queue-empty frontier) ;if queue has no nodes there is no solution and so we failed
                (return-from best-first-search NIL)
                (setf node (p-queue-pop frontier)))
            (cond ((eq (funcall (problema-solucao p) (first node)) T) ;If we found a solution
                (format T ">>> Nodes expanded: ~A" nodesExpanded)
				(write-line "")
                (format T ">>> Nodes generated: ~A" nodesGenerated)
				(write-line "")
                (return-from best-first-search (first node)))
				;(return-from best-first-search 'LISTA_DE_ACCOES))
            (T  
                (let ((newE NIL))
                    (dolist (a (funcall (problema-accoes p) (first node)))
						(incf nodesGenerated)
                        (setf newE (funcall (problema-resultado p) (first node) a))
                        (p-queue-insert frontier newE (funcall F newE)))))))))
            
    

(defun procura-A* (p h)
    (best-first-search p #'(lambda(e) 
                            (+ (funcall (problema-custo-caminho p) e) (funcall h e)))))
   
(defun procura-gananciosa (p h)
	(best-first-search p #'(lambda(e) 
							(funcall h e))))

;(defun procura-best (array lista-pecas))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;TESTES;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun test(funcao ppc heu)
	(if (not (listp ppc)) (return-from test 'PPC_NOT_ALIST!!) NIL)
	(if (not (functionp heu)) (return-from test 'HEU_NOT_AFUN!!) NIL)
	(if (not (functionp funcao)) (return-from test 'HEU_NOT_AFUN!!) NIL)
	
	(write-line "")
	(write-line "")
	(write-line "")
	(write-line ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
	(write-line ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
	(write-line ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
	(format T "->Testing ~A:" funcao)
	(write-line "")
	(format T "->ppc: (~{~A~^, ~})" ppc)
	(write-line "")
	(format T "->heuristica: ~A!" heu)
	(write-line "")

	(let* ((t1 (cria-tabuleiro))
		  (e1 (make-estado :tabuleiro t1 :pecas-por-colocar ppc))
		  (p1 (make-problema :estado-inicial e1
				        :solucao #'solucao
				        :accoes #'accoes
				        :resultado #'resultado
				        :custo-caminho #'custo-oportunidade)))

	(funcall funcao p1 heu)))

;;;;;;;TESTED VALUES;;;;;;;;
;(print (time (testA* '(o o o o o) #'qualidade))) ;(15s, 842nodes)
;(print (time (testA* '(o o o o o) #'average-height-h))) ;(0.8s, 278nodes)
;(print (time (#'procura-A* '(i i i i) #'average-height-h))) ;(13880s, 2561nodes)
;(print (time (test #'procura-gananciosa '(o o o o o) #'(lambda(e) (+ (average-height-h e) (qualidade e)))))) ;(0.002s, 6nodes expanded, 32 nodes generated)



(load "utils.fas")


