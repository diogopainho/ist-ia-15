;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Grupo 38 - Alameda - 72471 Michael Santos - 73245 Diogo Painho - 75219 Joao Franco ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; UTILITY FUNTIONS ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; copia-array-2D: array --> array
;;; Funcao que recebe um array e devolve uma copia desse array.
(defun copia-array-2D (array1)
    (let ((array2 (make-array (array-dimensions array1))))
        (dotimes (i (array-dimension array1 0))
            (dotimes (j (array-dimension array1 1))
                (setf (aref array2 i j) (aref array1 i j))))
        array2))

;;; iguais-array-2D: array x array --> boolean
;;; Funcao que recebe dois arrays e retorna T caso estes sejam iguais e NIL caso contrario
;;; @Robustness: Nao testa se os arrays tem a mesma dimensao (como e usado para testar se dois tabuleiros sao iguais nao e necessario)
(defun iguais-array-2D(array1 array2)
	(dotimes (i (array-dimension array1 0))
        (dotimes (j (array-dimension array1 1))
            (if (not (eq (aref array1 i j) (aref array2 i j)))
                (return-from iguais-array-2D NIL))))
    T)

;;; equal-lists: list x list --> boolean
;;; Funcao que recebe duas listas e retorna T caso estas sejam iguais e NIL caso contrario
(defun equal-lists (lst1 lst2)
    (cond ((and (null lst1) (null lst2)) ;Se ambas sao nulas retorna t (iguais)
        t)
    ((or (null lst1) (null lst2)) ;Caso apenas uma das listas seja nula entao nao tem a mesma dimensao logo retorna NIL (nao iguais)
        NIL)
    (t
        (if (eq (car lst1) (car lst2)) ;Se os primeiros elementos sao iguais entao chama recursivamente com o resto da lista, se nao retorna NIL (nao iguais)
            (equal-lists (cdr lst1) (cdr lst2))
            NIL))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; MIN BINARY HEAP (PRIORITY QUEUE) ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct node
	element
	key)

(defun make-binary-heap (&optional (default-increment 512))
	(make-array default-increment :fill-pointer 1 :adjustable t))

(declaim (inline heap-parent))
(defun heap-parent (i)
	(floor i 2)) ; i/2

(declaim (inline heap-left))
(defun heap-left (i)
	(* 2 i)) ;2i

(declaim (inline heap-right))
(defun heap-right (i)
	(+ (* i 2) 1)) ;2i + 1

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
		(return-from heap-pop NIL))

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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO PRIORITY QUEUE ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; priority-queue: Estrutura que define uma priority-queue que guarda o tamanho da fila e elementos.
;;; Estes elementos sao um tuplo em que o primeiro e um elemento dado pelo utilizador quando insere na fila e o segundo 
;;; um inteiro que representa o valor desse elemento. E com este valor que a fila e organizada do menor valor para o maior.
(defstruct priority-queue
    (size 0)
    (pair NIL))

;;; p-queue-empty: priority-queue --> boolean
;;; Esta funcao retorna T caso a fila esteja vazia e NIL caso contrario
(defun p-queue-empty (pq)
    (if (zerop (priority-queue-size pq))
        T
        NIL))

;;; p-queue-pop: priority-queue --> element
;;; Esta funcao retira o primeiro tuplo da fila e retorna o elemento associado a esse tuplo.
;;; @Robustness: should test if queue is empty before poping?
(defun p-queue-pop (pq)
    (decf (priority-queue-size pq))
    (first (pop (priority-queue-pair pq))))

;;; p-queue-insert: priority-queue x elemento x inteiro --> {}
;;; Esta funcao insere um tuplo com o elemento e o valor dado nos argumentos pelo utilizador.
;;; Este tuplo e inserido na fila de acordo com o seu valor, de modo a que a fila fique organizado do mais pequeno para o maior.
;;; Em caso de empate de valores o ultimo a ser inserido fica mais a frenta na fila.
(defun p-queue-insert (pq newEle newVal)
    (cond ((p-queue-empty pq) ;Caso seja o nosso primeiro insert (isto e a fila esta vazia)
        (incf (priority-queue-size pq))
        (setf (priority-queue-pair pq) (list (list newEle newVal))))
    (T
        (let ((i 0))
            (loop
                (when (>= i (priority-queue-size pq)) (return)) ;Caso tenhamos chegado ao fim da fila break (caso o valor dado seja maior que todos os valores na fila)
                (when (>= (second (nth i (priority-queue-pair pq))) newVal) (return)) ;Caso o valor que esta a ser iterado na fila seja >= que o valor dado - break.
                (incf i))
            
			(incf (priority-queue-size pq))
            (cond ((eq i 0) ;colocar tuplo no inicio da lista
                (setf (priority-queue-pair pq) (cons (list newEle newVal) (priority-queue-pair pq))))
            (T
                (push (list newEle newVal) (cdr (nthcdr (1- i) (priority-queue-pair pq))))))))))


    
;;;;;;;;;;;;;;;;;;;;
;;;; TIPO ACCAO ;;;;
;;;;;;;;;;;;;;;;;;;;

;;; cria-accao: inteiro x peca --> accao
;;; Funcao que recebe um inteiro que representa o numero da coluna e uma peca e retorna uma accao correspondente (um par com os dois elementos).
(defun cria-accao (coluna peca)
  (cons coluna peca))

;;; accao-coluna: accao --> inteiro
;;; Selector que recebe uma accao e devolve a coluna mais a esquerda a partir da qual a peca vai ser colocada.
(defun accao-coluna (a)
  (car a))

;;; accao-peca: accao --> peca
;;; Selector que recebe uma accao e devolve a peca correspondente a essa accao.
(defun accao-peca (a)
  (cdr a))



;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO TABULEIRO ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; colunas: Constante que representa o numero de colunas de um tabuleiro.
(defconstant colunas 10)

;;; linhas: Constante que representa o numero de linhas de um tabuleiro.
(defconstant linhas 18)

;;; tabuleiro: Estrutura que define o tabuleiro como um array bi-dimensional, em que cada posicao representa uma casa do tabuleiro.
(defstruct tabuleiro
    array)

;;; cria-tabuleiro: {} --> tabuleiro
;;; Construtor que devolve um tabuleiro vazio.
(defun cria-tabuleiro ()
    (make-tabuleiro :array (make-array (list linhas colunas) :initial-element NIL)))

;;; copia-tabuleiro: tabuleiro --> tabuleiro
;;; Construtor de copia que recebe um tabuleiro e devolve um novo tabuleiro com o mesmo conteudo do tabuleiro recebido. 
;;; E garantido que qualquer alteracao nao se propaga para o tabuleiro original.
(defun copia-tabuleiro (tab1)
    (let ((tab2 (copy-tabuleiro tab1)))
		(setf (tabuleiro-array tab2) (copia-array-2D (tabuleiro-array tab1)))
        tab2))

;;; tabuleiros-iguais-p: tabuleiro x tabuleiro --> boolean
;;; Funcao que receve dois tabuleiros e devolve T caso sejam iguais e NIL caso contrario.
(defun tabuleiros-iguais-p (tab1 tab2)
    (iguais-array-2D (tabuleiro-array tab1) (tabuleiro-array tab2)))

;;; tabuleiro->array: tabuleiro --> array
;;; Transformador de saida que recebe um tabuleiro e devolve um novo array com 18 linhas e 10 colunas em que cada linha e coluna contem 
;;; o valor logico correspondente a cada posicao do tabuleiro.
(defun tabuleiro->array (tab)
    (copia-array-2D (tabuleiro-array tab)))

;;; array->tabuleiro: array --> tabuleiro
;;; Transformador de entrada que recebe um array de 18 linhas e 10 colunas cujas posicoes tem o valor logico T ou NIL e 
;;; constroi um novo tabuleiro com o conteudo do array recebido.
(defun array->tabuleiro (array)
    (let ((tab (cria-tabuleiro)))
        (setf (tabuleiro-array tab) (copia-array-2D array))
        tab))

;;; tabuleiro-preenchido-p: tabuleiro x inteiro x inteiro --> boolean
;;; Funcao que recebe um tabuleiro, um inteiro correspondente a linha e um inteiro corresponte a coluna.
;;; Devolve T caso a posicao correspondente esteja preenchida e NIL caso contrario.
(defun tabuleiro-preenchido-p (tab lin col)
    (if (eq (aref (tabuleiro-array tab) lin col) NIL)
        NIL
        t))

;;; tabuleiro-linha-completa-p: tabuleiro x inteiro --> boolean
;;; Reconhecedor que recebe um tabuleiro e um inteiro correspondente a uma linha.
;;; Devolve T se todas as posicoes da linha estiverem preenchidas e NIL caso contrario.
(defun tabuleiro-linha-completa-p (tab lin)
    (let ((i (- colunas 1)))
		(loop
			(when (= i -1) (return t))
			(when (not (tabuleiro-preenchido-p tab lin i)) (return NIL))
			(setf i (- i 1)))))

;;; tabuleiro-topo-preenchido-p: tabuleiro --> boolean
;;; Reconhecedor que recebe um tabuleiro e devolve o T se todas as posicoes do topo do tabuleiro estiverem preenchidas e NIL caso contrario.
(defun tabuleiro-topo-preenchido-p (tab)
    (let ((i (- colunas 1)))
		(loop
			(when (= i -1) (return NIL))
			(when (tabuleiro-preenchido-p tab (- linhas 1) i) (return t))
			(setf i (- i 1)))))

;;; tabuleiro-altura-coluna: tabuleiro x inteiro --> inteiro
;;; Funcao que recebe um tabuleiro e um inteiro correspondente a uma coluna.
;;; Devolve um inteiro corresponte a altura dessa mesma coluna, ou seja a posicao mais alta que esta preenchida nessa coluna.
(defun tabuleiro-altura-coluna (tab col)
    (do ((i (- linhas 1) (- i 1)))
        ((or (= i -1) (tabuleiro-preenchido-p tab i col)) (+ i 1))))

;;; tabuleiro-buracos-coluna: tabuleiro x inteiro --> inteiro
;;; Funcao que recebe um tabuleiro e um inteiro correspondente ao numero de coluna e
;;; devolve o numero de "buracos" nessa coluna. Um "buraco" e um espaco em branco na coluna
;;; quando ainda existem posicoes acima ocupadas.
(defun tabuleiro-buracos-coluna (tab col)
	 (let ((holeAmount 0) (foundEmpty NIL))
		(dotimes (i linhas)
			(cond ((and (eq foundEmpty NIL) (eq (tabuleiro-preenchido-p tab i col) NIL)) ;Se nunca encontrou uma casa vazia e agora encontrou
				(setf foundEmpty T))
			((and (eq foundEmpty T) (eq (tabuleiro-preenchido-p tab i col) T)) ;Se encontrou empty e agora a casa esta preenchida entao temos um buraco
				(incf holeAmount)
				(setf foundEmpty NIL))))
		holeAmount))

;;; tabuleiro-preenche!: tabuleiro x inteiro x inteiro --> {}
;;; Modificador que recebe um tabuleiro, um inteiro correspondente ao numero da linha e um inteiro correspondente ao numero da coluna e
;;; preenche a posicao dada no tabuleiro.
(defun tabuleiro-preenche! (tab lin col)
    (cond ((or (>= lin linhas) (>= col colunas))  ;caso linhas ou colunas estejam fora do tabuleiro devolve msg de erro
		'("error: tabuleiro-preenche"))
    (t
        (setf (aref (tabuleiro-array tab) lin col) t))))

;;; tabuleiro-remove-linha!: tabuleiro x inteiro --> {}
;;; Modificador que recebe um tabuleiro e um inteiro correspondente ao numero da linha e altera o tabuleiro removendo a linha correspondente. 
;;; As linhas acima descem uma linha e as linhas que estao por baixo nao sao alteradas.
(defun tabuleiro-remove-linha! (tab lin)
    (do ((i lin (+ i 1)))
        ((= i (- linhas 1)) NIL)
        (dotimes (j colunas)
            (setf (aref (tabuleiro-array tab) i j) (aref (tabuleiro-array tab) (+ i 1) j))))
    (dotimes (i colunas)
        (setf (aref (tabuleiro-array tab) (- linhas 1) i) NIL)))

		

;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO ESTADO ;;;;
;;;;;;;;;;;;;;;;;;;;;

;;; estado: Estrutura que define o estado com tendo pontos, pecas por colocar, pecas colocadas e um tabuleiro
(defstruct estado
    (pontos 0)
    pecas-por-colocar
    pecas-colocadas ;lista ordenada da peca mas recente para a mais antiga
    tabuleiro)

;;; copia-estado: estado --> estado
;;; Construtor que recebe um estado e devolve uma copia do estado recebido.
(defun copia-estado (estado1)
    (let ((estado2 (copy-estado estado1)))
        (setf (estado-tabuleiro estado2) (copia-tabuleiro (estado-tabuleiro estado1)))
        (setf (estado-pecas-por-colocar estado2) (copy-list (estado-pecas-por-colocar estado1)))
        (setf (estado-pecas-colocadas estado2) (copy-list (estado-pecas-colocadas estado1)))
        estado2))

;;; estados-iguais-p: estado x estado --> boolean
;;; Teste que recebe dois estados e devolve T caso os estados sejam iguais ou NIL caso contrario
(defun estados-iguais-p (e1 e2)
    (cond ((and (= (estado-pontos e1) (estado-pontos e2))
               (tabuleiros-iguais-p (estado-tabuleiro e1) (estado-tabuleiro e2))
               (equal-lists (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2))
               (equal-lists (estado-pecas-colocadas e1) (estado-pecas-colocadas e2)))
        t)
    (t
        NIL)))

;;; estado-final-p: estado --> boolean
;;; Reconhecedor que recebe um estado e devolve T se corresponder a um estado final e NIL caso contrario.
;;; Um estado e final se uma peca tiver atingido o topo do tabuleiro ou se ja nao existirem pecas por colocar.
(defun estado-final-p (estado)
    (if (or (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) (null (estado-pecas-por-colocar estado))) 
		t 
		NIL))

;;; estado-terminal: estado --> boolean
;;; Reconhecedor que recebe um estado e devolve T caso o estado seja terminal e NIL caso contrario.
;;; Um estado e terminal quando uma peca ja atingiu o topo do tabuleiro.
(defun estado-terminal (estado)
    (if (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) 
		t 
		NIL))



;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO PROBLEMA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

;;; problema: Estrutura que define um problema como tendo estado-inicial, solucao, accoes, resultado e o custo-caminho
(defstruct problema
    estado-inicial
    solucao
    accoes
    resultado
    custo-caminho)

;;; solucao: estado --> boolean
;;; Funcao que recebe um estado e devolve T se o estado recebido corresponder a uma solucao e NIL caso constrario
;;; Um estado do jogo Tetris e considerado solucao se o topo do tabuleiro nao estiver preenchido e se ja nao existirem pecas por colocar,
;;; ou seja todas as pecas foram colocadas com sucesso.
(defun solucao (estado)
    (if (and (null (estado-pecas-por-colocar estado)) (not (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))))
        t
        NIL))

;;; accoes-aux-generico: lista-de-accoes x array --> lista-de-accoes
;;; Funcao auxiliar a 'accoes-generico' que recebe uma lista de accoes e um array que reprenta a configuracao de uma peca.
;;; Devolve a lista de accoes para a configuracao da peca recebida concatenada com a lista de accoes recebida nos argumentos.
(defun accoes-aux-generico (lst-accoes array-peca)
    (dotimes (i (- (+ colunas 1) (array-dimension array-peca 1)))
        (setf lst-accoes (append lst-accoes (list (cria-accao i array-peca)))))
    lst-accoes)

;;; accoes-generico: estado --> lista-de-accoes
;;; Funcao que recebe um estado e devolve uma lista de accoes correspondendo a todas as accoes validas que podem ser feitas com a proxima peca a ser colocada.
;;; Esta funcao e mais generica e consequentemente mais lenta do que a funcao usada 'accoes', mas e mais facil para gerar 
;;; valores caso por exemplo uma peca nova tenha de ser adicionada.
(defun accoes-generico (estado) 
	(when (eq (estado-terminal estado) t)
		(return-from accoes-generico NIL))

    (let ((peca (first (estado-pecas-por-colocar estado)))
          (lst-accoes NIL))
		   
        (cond ((eq peca 't) (setf lst-accoes (accoes-aux lst-accoes peca-t0))
                            (setf lst-accoes (accoes-aux lst-accoes peca-t1))
                            (setf lst-accoes (accoes-aux lst-accoes peca-t2))
                            (setf lst-accoes (accoes-aux lst-accoes peca-t3)))
              ((eq peca 'l) (setf lst-accoes (accoes-aux lst-accoes peca-l0))
                            (setf lst-accoes (accoes-aux lst-accoes peca-l1))
                            (setf lst-accoes (accoes-aux lst-accoes peca-l2))
                            (setf lst-accoes (accoes-aux lst-accoes peca-l3)))
              ((eq peca 'j) (setf lst-accoes (accoes-aux lst-accoes peca-j0))
                            (setf lst-accoes (accoes-aux lst-accoes peca-j1))
                            (setf lst-accoes (accoes-aux lst-accoes peca-j2))
                            (setf lst-accoes (accoes-aux lst-accoes peca-j3)))
              ((eq peca 'i) (setf lst-accoes (accoes-aux lst-accoes peca-i0))
                            (setf lst-accoes (accoes-aux lst-accoes peca-i1)))
              ((eq peca 's) (setf lst-accoes (accoes-aux lst-accoes peca-s0))
                            (setf lst-accoes (accoes-aux lst-accoes peca-s1)))
              ((eq peca 'z) (setf lst-accoes (accoes-aux lst-accoes peca-z0))
                            (setf lst-accoes (accoes-aux lst-accoes peca-z1)))
              ((eq peca 'o) (setf lst-accoes (accoes-aux lst-accoes peca-o0))))))

;;; Constantes: Cada uma destas constantes representam uma lista que guarda todas as accoes possiveis com cada peca que lhes corresponde.
;;; Embora fique mais 'feio' torna o resolucao dos problemas mais eficiente uma vez que nao tem de recalcular valores constantetmente,
;;; e desta forma poupa-se tempo e espaco. Estes valores foram gerados com uma funcao generica mais lenta 'accoes-genericas'.
(defconstant accoes-t '((0 . #2A((T T T) (NIL T NIL)))   (1 . #2A((T T T) (NIL T NIL)))   (2 . #2A((T T T) (NIL T NIL)))   (3 . #2A((T T T) (NIL T NIL)))   (4 . #2A((T T T) (NIL T NIL)))               
						(5 . #2A((T T T) (NIL T NIL)))   (6 . #2A((T T T) (NIL T NIL)))   (7 . #2A((T T T) (NIL T NIL)))   (0 . #2A((T NIL) (T T) (T NIL))) (1 . #2A((T NIL) (T T) (T NIL)))           
						(2 . #2A((T NIL) (T T) (T NIL))) (3 . #2A((T NIL) (T T) (T NIL))) (4 . #2A((T NIL) (T T) (T NIL))) (5 . #2A((T NIL) (T T) (T NIL))) (6 . #2A((T NIL) (T T) (T NIL)))     
						(7 . #2A((T NIL) (T T) (T NIL))) (8 . #2A((T NIL) (T T) (T NIL))) (0 . #2A((NIL T NIL) (T T T)))   (1 . #2A((NIL T NIL) (T T T)))   (2 . #2A((NIL T NIL) (T T T)))           
						(3 . #2A((NIL T NIL) (T T T)))   (4 . #2A((NIL T NIL) (T T T)))   (5 . #2A((NIL T NIL) (T T T)))   (6 . #2A((NIL T NIL) (T T T)))   (7 . #2A((NIL T NIL) (T T T)))               
						(0 . #2A((NIL T) (T T) (NIL T))) (1 . #2A((NIL T) (T T) (NIL T))) (2 . #2A((NIL T) (T T) (NIL T))) (3 . #2A((NIL T) (T T) (NIL T))) (4 . #2A((NIL T) (T T) (NIL T)))     
						(5 . #2A((NIL T) (T T) (NIL T))) (6 . #2A((NIL T) (T T) (NIL T))) (7 . #2A((NIL T) (T T) (NIL T))) (8 . #2A((NIL T) (T T) (NIL T)))))

(defconstant accoes-l '((0 . #2A((T T) (T NIL) (T NIL))) (1 . #2A((T T) (T NIL) (T NIL))) (2 . #2A((T T) (T NIL) (T NIL))) (3 . #2A((T T) (T NIL) (T NIL))) (4 . #2A((T T) (T NIL) (T NIL)))     
						(5 . #2A((T T) (T NIL) (T NIL))) (6 . #2A((T T) (T NIL) (T NIL))) (7 . #2A((T T) (T NIL) (T NIL))) (8 . #2A((T T) (T NIL) (T NIL))) (0 . #2A((T NIL NIL) (T T T)))       
						(1 . #2A((T NIL NIL) (T T T)))   (2 . #2A((T NIL NIL) (T T T)))   (3 . #2A((T NIL NIL) (T T T)))   (4 . #2A((T NIL NIL) (T T T)))   (5 . #2A((T NIL NIL) (T T T)))               
						(6 . #2A((T NIL NIL) (T T T)))   (7 . #2A((T NIL NIL) (T T T)))   (0 . #2A((NIL T) (NIL T) (T T))) (1 . #2A((NIL T) (NIL T) (T T))) (2 . #2A((NIL T) (NIL T) (T T)))         
						(3 . #2A((NIL T) (NIL T) (T T))) (4 . #2A((NIL T) (NIL T) (T T))) (5 . #2A((NIL T) (NIL T) (T T))) (6 . #2A((NIL T) (NIL T) (T T))) (7 . #2A((NIL T) (NIL T) (T T)))     
						(8 . #2A((NIL T) (NIL T) (T T))) (0 . #2A((T T T) (NIL NIL T)))   (1 . #2A((T T T) (NIL NIL T)))   (2 . #2A((T T T) (NIL NIL T)))   (3 . #2A((T T T) (NIL NIL T)))             
						(4 . #2A((T T T) (NIL NIL T)))   (5 . #2A((T T T) (NIL NIL T)))   (6 . #2A((T T T) (NIL NIL T)))   (7 . #2A((T T T) (NIL NIL T)))))

(defconstant accoes-j '((0 . #2A((T T) (NIL T) (NIL T))) (1 . #2A((T T) (NIL T) (NIL T))) (2 . #2A((T T) (NIL T) (NIL T))) (3 . #2A((T T) (NIL T) (NIL T))) (4 . #2A((T T) (NIL T) (NIL T)))     
						(5 . #2A((T T) (NIL T) (NIL T))) (6 . #2A((T T) (NIL T) (NIL T))) (7 . #2A((T T) (NIL T) (NIL T))) (8 . #2A((T T) (NIL T) (NIL T))) (0 . #2A((T T T) (T NIL NIL)))       
						(1 . #2A((T T T) (T NIL NIL)))   (2 . #2A((T T T) (T NIL NIL)))   (3 . #2A((T T T) (T NIL NIL)))   (4 . #2A((T T T) (T NIL NIL)))   (5 . #2A((T T T) (T NIL NIL)))               
						(6 . #2A((T T T) (T NIL NIL)))   (7 . #2A((T T T) (T NIL NIL)))   (0 . #2A((T NIL) (T NIL) (T T))) (1 . #2A((T NIL) (T NIL) (T T))) (2 . #2A((T NIL) (T NIL) (T T)))         
						(3 . #2A((T NIL) (T NIL) (T T))) (4 . #2A((T NIL) (T NIL) (T T))) (5 . #2A((T NIL) (T NIL) (T T))) (6 . #2A((T NIL) (T NIL) (T T))) (7 . #2A((T NIL) (T NIL) (T T)))     
						(8 . #2A((T NIL) (T NIL) (T T))) (0 . #2A((NIL NIL T) (T T T)))   (1 . #2A((NIL NIL T) (T T T)))   (2 . #2A((NIL NIL T) (T T T)))   (3 . #2A((NIL NIL T) (T T T)))             
						(4 . #2A((NIL NIL T) (T T T)))   (5 . #2A((NIL NIL T) (T T T)))   (6 . #2A((NIL NIL T) (T T T)))   (7 . #2A((NIL NIL T) (T T T)))))

(defconstant accoes-i '((0 . #2A((T) (T) (T) (T))) (1 . #2A((T) (T) (T) (T))) (2 . #2A((T) (T) (T) (T))) (3 . #2A((T) (T) (T) (T))) (4 . #2A((T) (T) (T) (T))) (5 . #2A((T) (T) (T) (T)))        
						(6 . #2A((T) (T) (T) (T))) (7 . #2A((T) (T) (T) (T))) (8 . #2A((T) (T) (T) (T))) (9 . #2A((T) (T) (T) (T))) (0 . #2A((T T T T)))       (1 . #2A((T T T T)))                    
						(2 . #2A((T T T T)))       (3 . #2A((T T T T)))       (4 . #2A((T T T T)))       (5 . #2A((T T T T)))       (6 . #2A((T T T T)))))

(defconstant accoes-s '((0 . #2A((T T NIL) (NIL T T)))   (1 . #2A((T T NIL) (NIL T T)))   (2 . #2A((T T NIL) (NIL T T)))   (3 . #2A((T T NIL) (NIL T T)))   (4 . #2A((T T NIL) (NIL T T)))               
						(5 . #2A((T T NIL) (NIL T T)))   (6 . #2A((T T NIL) (NIL T T)))   (7 . #2A((T T NIL) (NIL T T)))   (0 . #2A((NIL T) (T T) (T NIL))) (1 . #2A((NIL T) (T T) (T NIL)))           
						(2 . #2A((NIL T) (T T) (T NIL))) (3 . #2A((NIL T) (T T) (T NIL))) (4 . #2A((NIL T) (T T) (T NIL))) (5 . #2A((NIL T) (T T) (T NIL))) (6 . #2A((NIL T) (T T) (T NIL)))     
						(7 . #2A((NIL T) (T T) (T NIL))) (8 . #2A((NIL T) (T T) (T NIL)))))
 
(defconstant accoes-z '((0 . #2A((NIL T T) (T T NIL)))   (1 . #2A((NIL T T) (T T NIL)))   (2 . #2A((NIL T T) (T T NIL)))   (3 . #2A((NIL T T) (T T NIL)))   (4 . #2A((NIL T T) (T T NIL)))               
						(5 . #2A((NIL T T) (T T NIL)))   (6 . #2A((NIL T T) (T T NIL)))   (7 . #2A((NIL T T) (T T NIL)))   (0 . #2A((T NIL) (T T) (NIL T))) (1 . #2A((T NIL) (T T) (NIL T)))           
						(2 . #2A((T NIL) (T T) (NIL T))) (3 . #2A((T NIL) (T T) (NIL T))) (4 . #2A((T NIL) (T T) (NIL T))) (5 . #2A((T NIL) (T T) (NIL T))) (6 . #2A((T NIL) (T T) (NIL T)))     
						(7 . #2A((T NIL) (T T) (NIL T))) (8 . #2A((T NIL) (T T) (NIL T)))))

(defconstant accoes-o '((0 . #2A((T T) (T T))) (1 . #2A((T T) (T T))) (2 . #2A((T T) (T T))) (3 . #2A((T T) (T T))) (4 . #2A((T T) (T T))) (5 . #2A((T T) (T T))) (6 . #2A((T T) (T T)))         
						(7 . #2A((T T) (T T))) (8 . #2A((T T) (T T)))))

;;; accoes: estado --> lista-de-accoes
;;; Funcao que recebe um estado e devolve uma lista de accoes correspondendo a todas as accoes validas que podem ser feitas com a proxima peca a ser colocada.
;;; Esta funcao e menos generica usando as constantes acima definidas pelo que e mais eficiente, com melhorias de tempo (e espaco) consideravies o suficiente 
;;; para tomarmos a decisao de a escolher na vez da funcao 'accoes-generico'.
(defun accoes (estado)
	(when (eq (estado-terminal estado) t) ;Caso o estado seja terminal retornar imediatamente NIL.
		(return-from accoes NIL))
	
	(let ((peca (first (estado-pecas-por-colocar estado))))
        (cond ((eq peca 't) accoes-t)
              ((eq peca 'l) accoes-l)
              ((eq peca 'j) accoes-j)
              ((eq peca 'i) accoes-i)
              ((eq peca 's) accoes-s)
              ((eq peca 'z) accoes-z)
              ((eq peca 'o) accoes-o))))

;;; altura-inversa-peca: array x inteiro --> inteiro
;;; Funcao auxiliar de resultado que recebe um array que representa uma peca e um inteiro que corresponde a uma coluna
;;; e devolve a 'altura inversa' da peca recebida na coluna recebida, isto e a altura da peca da peca a contar de baixo.
(defun altura-inversa-peca (peca col)
    (do ((i 0 (incf i)))
        ((or (= i (array-dimension peca 1)) (eq (aref peca i col) T)) 
			i)))

;;; resultado: estado x accao --> estado
;;; Funcao que recebe um estado e uma accao e devolve um novo estado que resulta de aplicar a accao recebida no estado recebido.
;;; O estado original fica inalterado.
(defun resultado (estado accao)
    (let ((e (copia-estado estado)))
        (setf (estado-pecas-colocadas e) (cons (first (estado-pecas-por-colocar e)) (estado-pecas-colocadas e))) ;atualiza pecas colocadas
        (setf (estado-pecas-por-colocar e) (rest (estado-pecas-por-colocar e))) ;atualiza pecas por colocar
        (let ((altura-temp 0) (primeira-linha 0))

            ;Calcula primeira-linha
            (dotimes (col (array-dimension (accao-peca accao) 1))
				;altura-temp = altura-tabuleiro(coluna=coluna-da-accao + i) - altura da peca a contar de baixo)
                (setf altura-temp (- (tabuleiro-altura-coluna (estado-tabuleiro e) (+ col (accao-coluna accao))) (altura-inversa-peca (accao-peca accao) col)))
                (cond ((> altura-temp primeira-linha) ;primeira-linha = max(altura-temp)
                    (setf primeira-linha altura-temp))))

            ;Preenche tabuleiro com a peca
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
                (let ((linhas-apagadas 0))
					(let* ((limite-inferior primeira-linha)
                           (limite-superior (+ (array-dimension (accao-peca accao) 0) primeira-linha))
					       (lin limite-superior))
						
						(loop 
							(when (eq (tabuleiro-linha-completa-p (estado-tabuleiro e) lin) T)
								(tabuleiro-remove-linha! (estado-tabuleiro e) lin)
								(setf linhas-apagadas (incf linhas-apagadas)))
							(when (= lin limite-inferior) (return))
							(decf lin)))

                    (cond ((= linhas-apagadas 0)
                        t)
                    ((= linhas-apagadas 1) (setf (estado-pontos e) (+ (estado-pontos e) 100)))
                    ((= linhas-apagadas 2) (setf (estado-pontos e) (+ (estado-pontos e) 300)))
                    ((= linhas-apagadas 3) (setf (estado-pontos e) (+ (estado-pontos e) 500)))
                    ((= linhas-apagadas 4) (setf (estado-pontos e) (+ (estado-pontos e) 800)))
                    (t (write-line "*******************PreMIO HACkeR(impossivel remover mais que 5 linhas)****************")))))))
        e))



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FUNCOES DE CUSTO ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; custo-oportunidade: estado --> inteiro
;;; Funcao que recebe um estado e devolve o custo de oportunidade de todas as accoes realizadas ate ao momento.
;;; Isto e o maximo de pontos possiveis ate ao momento subtraindo os pontos ja obtidos no estado recebido.
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



;;;;;;;;;;;;;;;;;;;;;
;;;; HEURISTICAS ;;;;
;;;;;;;;;;;;;;;;;;;;;

;;; qualidade: estado --> inteiro
;;; Funcao que recebe um estado e devolve um inetiro correspondente ao valor de qualidade do estado.
;;; O valor de qualidade e o valor negativo dos pontos ganhos ate no respectivo estado.
(defun qualidade (estado)
    (* (estado-pontos estado) -1))
	
;;; leveled-state-h: estado --> inteiro
;;; Funcao heuristica que recebe um estado e devolve a diferenca entre o nivel maximo e o nivel medio das pecas.
(defun leveled-state-h (e)
    (let ((currColH NIL) 
	      (maxH 0) 
		  (average 0))
        (dotimes (i colunas)
            (setf currColH (tabuleiro-altura-coluna (estado-tabuleiro e) i))
            (when (> currColH maxH) (setf maxH currColH))
			(setf average (+ average currColH)))
		(setf average (round average colunas))
		(- maxH average)))

;;; holes-h: estado --> inteiro
;;; Funcao heuristica que recebe um estado o numero de buracos que esse estado tem.
;;; Um "buraco" e um espaco em branco na coluna quando ainda existem posicoes acima ocupadas.
(defun holes-h (e)
    (let ((holeAmount 0))
        (dotimes (i colunas)
			(setf holeAmount (+ holeAmount (tabuleiro-buracos-coluna (estado-tabuleiro e) i))))
		holeAmount))



;;;;;;;;;;;;;;;;;
;;;; PROCURA ;;;;
;;;;;;;;;;;;;;;;;

;;; procura-pp: problema --> lista-de-accoes
;;; Esta funcao recebe um problema e usa a procura em profundidade primeiro em arvore (DFS) e 
;;; devolve uma lista de accoes correspondente ao caminho que resolve o problema recebido.
(defun procura-pp (p)
    (recursive-pp p))

;;; recursive-pp: problema --> lista-de-accoes
;;; Igual a procura-pp. Usa uma procura de forma recursiva.
(defun recursive-pp (p)
    (cdr (recursive-pp-aux p (problema-estado-inicial p) NIL)))

;;; recursive-pp-aux: problema x estado x accao --> lista-de-accoes
;;; Funcao auxiliar a 'recursive-pp' que recebe um problema, um estado e uma accao. Esta funcao vai devolver NIL se o estado 
;;; que recebe nao for solucao ou caso contrario a lista de accoes que levaram a gerar o tal estado.
(defun recursive-pp-aux (p e oldA)
    (cond ((eq (funcall (problema-solucao p) e) T) ;caso estado recebido seja solucao do problema
        (list oldA))
    (T
        (let ((result NIL))
            (dolist (a (reverse (funcall (problema-accoes p) e))) ;Tivemos de reverter a lista para passar no Mooshak
                (setf result (recursive-pp-aux p (funcall (problema-resultado p) e a) a))
                (when (not (eq result NIL))
                    (return-from recursive-pp-aux (cons oldA result)))))
        NIL)))

;;; search-node Estrutura que guarda um estado e a lista de accoes tomadas para alcancar o mesmo.
(defstruct search-node
	estado
	lst-accoes)

;;; best-first-search: problema x (funcao: estado --> inteiro) --> lista-de-accoes
;;; Funcao generica que recebe um problema e uma funcao que atribui um valor a um dado estado e devolve uma lista de accoes
;;; corresponde ao caminho que resolve o problema recebido. A funcao e usada para orientar o algoritmo no caminho certo.
(defun best-first-search (p F)
    (let ((node NIL) 
          (frontier (make-priority-queue))
          (nodesExpanded 0)
		  (nodesGenerated 0))
        (p-queue-insert frontier (make-search-node :estado (problema-estado-inicial p) :lst-accoes (list NIL))
								 (funcall F (problema-estado-inicial p)))
        (loop
            (incf nodesExpanded)

            (when (p-queue-empty frontier) ;caso a fila esteja vazia entao o problema nao tem solucao
                (return-from best-first-search NIL))
            
			(setf node (p-queue-pop frontier))

            (cond ((eq (funcall (problema-solucao p) (search-node-estado node)) T) ;Caso tenhamos encontrado uma solucao
                ;(format T ">>> Nodes expanded: ~A" nodesExpanded)
				;(write-line "")
                ;(format T ">>> Nodes generated: ~A" nodesGenerated)
				;(write-line "")
                (return-from best-first-search (cdr (search-node-lst-accoes node)))) ;cdr para remover o NIL inicialmente colocado
            (T  
                (let ((newE NIL) (prev-accoes (search-node-lst-accoes node)))
                    (dolist (a (funcall (problema-accoes p) (search-node-estado node)))
						(incf nodesGenerated)
                        (setf newE (funcall (problema-resultado p) (search-node-estado node) a))
                        (p-queue-insert frontier (make-search-node :estado newE :lst-accoes (append prev-accoes (list a)))
												 (funcall F newE)))))))))

;;; procura-A*: problema x (funcao: estado --> inteiro) --> lista-de-accoes
;;; Funcao que recebe um problema e uma funcao heuristica que atribui um valor heuristico a um dado estado e devolve uma lista de accoes
;;; corresponde ao caminho que resolve o problema recebido. Para orientar a funcao usa a funcao de custo (g) e a funcao heuristica
;;; recebida (h) somadas (f = g + h).
(defun procura-A* (p h)
    (best-first-search p #'(lambda(e) 
                            (+ (funcall (problema-custo-caminho p) e) (funcall h e)))))

;;; procura-gananciosa: problema x (funcao: estado --> inteiro) --> lista-de-accoes
;;; Funcao que recebe um problema e uma funcao heuristica que atribui um valor heuristico a um dado estado e devolve uma lista de accoes
;;; corresponde ao caminho que resolve o problema recebido. Para orientar a funcao usa a funcao heuristica recebida (h) (f = h).
(defun procura-gananciosa (p h)
	(best-first-search p #'(lambda(e)  
							(funcall h e))))

;;; procura-best: array x lista-pecas --> lista-de-accoes
;;; Funcao que recebe um array correspondente ao tabuleiro e uma lista de pecas por colocar. A funcao inicaliza um problema com 
;;; estes argumentos e devolve a melhor solucao, na forma de lista de accoes, dentro de um tempo aceitavel. Para tal combina 
;;; @TODO: acabar comentario
(defun procura-best (tabArray lista-pecas)
	(let* ((t1 (array->tabuleiro tabArray))
		   (e1 (make-estado :pontos 0
			     		    :pecas-por-colocar lista-pecas
						    :pecas-colocadas NIL
						    :tabuleiro t1))
		   (p1 (make-problema :estado-inicial e1
							  :solucao #'solucao
							  :accoes #'accoes
							  :resultado #'resultado
							  :custo-caminho #'custo-oportunidade)))
        (procura-A* p1 #'(lambda(e) (+ (leveled-state-h e) (qualidade e) (* 100 (holes-h e)))))))



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
;(print (time (testA* '(o o o o o) #'leveled-state-h))) ;(0.8s, 278nodes)
;(print (time (#'procura-A* '(i i i i) #'leveled-state-h))) ;(13880s, 2561nodes)
;(print (time (test #'procura-gananciosa '(o o o o o) #'(lambda(e) (+ (leveled-state-h e) (qualidade e)))))) ;(0.002s, 6nodes expanded, 32 nodes generated)



(load "utils.fas")
