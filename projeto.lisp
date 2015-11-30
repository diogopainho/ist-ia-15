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
;;; Para a criacao desta binary-heap foi tido como referencia o livro:
;;; [Introduction to Algorithms, 3rd Edition] by Thomas H. Cormen, Charles E. Leiserson, Ronald L. Rivest, Clifford Stein

;;; heap-node: Uma estrutura existente em cada posicao do array (array esse que representa a binary-heap).
;;;		'element' (qualquer tipo): O elemento que o utilizador quer guardar.
;;;		'key' (inteiro): O valor de prioridade associado ao 'element'.
;;;		'order-stamp'(inteiro): Um valor que e diferente em qualquer 'heap-node' para permitir a comparacao em elementos com a mesma prioridade.
;;;								Os elementos colocados mais TARDE tem um valor MAIOR.
(defstruct heap-node
	element
	key
	order-stamp)

;;; make-binary-heap: inteiro (opcional) --> binary-heap
;;; Este construtor recebe um inteiro optional que representa o valor inicial do tamanho do array e devolve um array com
;;; tamanho ajustavel, com fill-pointer a 1 e com a posicao 0 com valor 0 (A[0] = 0).
;;; Este array e a representacao de uma binary-heap na qual o 'heap-node' raiz se encontra na posicao 1. 
;;; A posicao 0 e usada para manter um contador que guarda quantos elementos ja foram colocados desde sempre, sendo usado
;;; para atribuir valores sempre diferentes de order-stamp a novos elementos inseridos.
(defun make-binary-heap (&optional (initial-size 512))
	(let ((A (make-array initial-size :fill-pointer 1 :adjustable t)))
	    (setf (aref A 0) 0)
	    A))

;;; heap-parent: inteiro --> inteiro
;;; Funcao que recebe um inteiro correspondente a uma posicao do array (que representa a binary-heap) e
;;; devolve a posicao do array que corresponde a sua raiz (parent).
(declaim (inline heap-parent))
(defun heap-parent (i)
	(floor i 2)) ; i/2

;;; heap-left: inteiro --> inteiro
;;; Funcao que recebe um inteiro correspondente a uma posicao do array (que representa a binary-heap) e
;;; devolve a posicao do array que corresponde a sua folha esquerda.
(declaim (inline heap-left))
(defun heap-left (i)
	(* 2 i)) ;2i

;;; heap-right: inteiro --> inteiro
;;; Funcao que recebe um inteiro correspondente a uma posicao do array (que representa a binary-heap) e
;;; devolve a posicao do array que corresponde a sua folha direita.
(declaim (inline heap-right))
(defun heap-right (i)
	(+ (* i 2) 1)) ;2i + 1

;;; heap-last-pos: binary-heap --> inteiro
;;; Funcao que recebe uma binary-heap e devolve a ultima posicao do array (que a representa) que esta valida.
(declaim (inline heap-last-pos))
(defun heap-last-pos (A)
	(- (fill-pointer A) 1))

;;; next-order-stamp: binary-heap --> inteiro
;;; Funcao que recebe uma binary-heap e devolve o proximo identicador unico de order-stamp, incrementando 
;;; o contador (guardado em A[0]).
(declaim (inline next-order-stamp))
(defun next-order-stamp (A)
	(incf (aref A 0)))

;;; min-heapify: binary-heap x inteiro --> {}
;;; Funcao que recebe um binary-heap e um inteiro que representa a posicao no array (que representa a binary-heap) da
;;; raiz da sub-arvore que precisa de ser equilibrada. Recursivamente equilibra a heap ate estar na forma correcta (A[Parent(i)] < A[i]).
;;; Tem complexidade temporal O(log(N)).
(defun min-heapify (A i)
	(let ((l (heap-left i))
		  (r (heap-right i))
		  (smallest i))

		;Testa se a folha esquerda, l, tem uma key mais pequena que o i (parent). Se sim coloca l como smallest.
		;Caso tenham o mesmo valor de key fica como smallest aquele com order-stamp maior.
	    (cond ((<= l (heap-last-pos A)) 
	        (if (< (heap-node-key (aref A l)) (heap-node-key (aref A i)))
	            (setf smallest l)
	            (when (and (= (heap-node-key (aref A l)) (heap-node-key (aref A i))) (> (heap-node-order-stamp (aref A l)) (heap-node-order-stamp (aref A i))))
	                (setf smallest l)))))
		
		
		;Testa se a folha direita, r, tem uma key mais pequena que o smallest. Se sim coloca r como smallest.
		;Caso tenham o mesmo valor de key fica como smallest aquele com order-stamp maior.
		(cond ((<= r (heap-last-pos A))
		    (if (< (heap-node-key (aref A r)) (heap-node-key (aref A smallest)))
			    (setf smallest r)
		        (when (and (= (heap-node-key (aref A r)) (heap-node-key (aref A smallest))) (> (heap-node-order-stamp (aref A r)) (heap-node-order-stamp (aref A smallest))))
			        (setf smallest r)))))
    
		;Caso exista um menor que o i (parent), entao trocamos e fazemos heapify na sub-arvore abaixo onde estava o smallest.
		(when (/= smallest i)
			(rotatef (aref A i) (aref A smallest)) ;swap
			(min-heapify A smallest))))

;;; heap-pop: binary-heap --> heap-node-element
;;; Funcao que recebe um binary-heap e devolve o heap-node-element que tem associado o menor
;;; valor de prioridade. Mantem a heap consistente com chamada a heapify.
;;; Tem complexidade temporal O(log(N)).
(defun heap-pop (A)
	;Testa se estamos a tentar remover de uma heap sem elementos (underflow)
	(when (< (heap-last-pos A) 1) 
		(return-from heap-pop NIL))

	;Guarda 'min', o valor na primeira casa que deve ser retornado
	(let ((min (aref A 1)))
		(setf (aref A 1) (aref A (heap-last-pos A))) ;coloca na primeira casa o ultimo elemento
		(decf (fill-pointer A))
		(min-heapify A 1)
		(heap-node-element min)))

;;; heap-decrease-key: binary-heap x inteiro x inteiro --> {}
;;; Funcao que recebe uma binary-heap, um inteiro correspondente ao valor da posicao do elemento no array (que representa a heap)
;;; ao qual queremos decrementar a key e um inteiro correspondente ao novo valor da key. Mantem a heap consistente.
;;; Caso o valor recebido seja maior do que a key original uma mensagem de erro e imprimida e NIL e devolvido.
;;; Tem complexidade temporal O(log(N)).
(defun heap-decrease-key (A i key)
	;Testa se a key recebida e maior que a key original de i na heap
	(when (> key (heap-node-key (aref A i)))
		(write-line "heap-decrease-key: Key received not smaller than the original key.")
		(return-from heap-decrease-key NIL))

	;Atualiza o valor da key na posicao i
	(setf (heap-node-key (aref A i)) key)

	;Atualiza a heap para ficar consistente
	(loop while (and (> i 1) (>= (heap-node-key (aref A (heap-parent i))) (heap-node-key (aref A i)))) do
		(rotatef (aref A (heap-parent i)) (aref A i)) ;swap
		(setf i (heap-parent i))))

;;; infinite-positive-number: Constante que representa o maior numero possivel de representar com 32 bits.
(defconstant infinite-positive-number 2147483647)

;;; min-heap-insert: binary-heap x heap-node-element x inteiro ---> {}
;;; Funcao que recebe uma binary-heap um heap-node-element a inserir e um inteiro correspondente ao valor de prioridade (key)
;;; associado a esse elemento. O elemento e inserido. A heap mantem-se consistente.
;;; Tem complexidade temporal O(log(N)).
(defun min-heap-insert (A element key)
	(vector-push-extend (make-heap-node :element element :key infinite-positive-number :order-stamp (next-order-stamp A)) A) ;Test with increment to check performance
	(heap-decrease-key A (heap-last-pos A) key))

    

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

;;; accoes-aux: lista-de-accoes x array --> lista-de-accoes
;;; Funcao auxiliar a 'accoes-generico' que recebe uma lista de accoes e um array que reprenta a configuracao de uma peca.
;;; Devolve a lista de accoes para a configuracao da peca recebida concatenada com a lista de accoes recebida nos argumentos.
(defun accoes-aux (lst-accoes array-peca)
    (dotimes (i (- (+ colunas 1) (array-dimension array-peca 1)))
        (setf lst-accoes (append lst-accoes (list (cria-accao i array-peca)))))
    lst-accoes)

;;; accoes: estado --> lista-de-accoes
;;; Funcao que recebe um estado e devolve uma lista de accoes correspondendo a todas as accoes validas que podem ser feitas com a proxima peca a ser colocada.
(defun accoes (estado) 
	(when (eq (estado-terminal estado) t)
		(return-from accoes NIL))

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
(defun h2-leveled-state (e)
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
;;; Funcao heuristica que recebe um estado e devolve o numero de buracos que esse estado tem.
;;; Um "buraco" e um espaco em branco na coluna quando ainda existem posicoes acima ocupadas.
(defun h3-holes (e)
    (let ((holeAmount 0))
        (dotimes (i colunas)
			(setf holeAmount (+ holeAmount (tabuleiro-buracos-coluna (estado-tabuleiro e) i))))
		holeAmount))

;;; best-heuristic: estado --> inteiro
;;; Funcao heuristica que recebe um estado e devolve um inteiro que representa o valor que melhor indica a 'qualidade' do estado recebido.
;;; Para tal devolve a diferenca entre o nivel maximo e medio das pecas * 300 caso seja maior que 4 mais o numero de buracos * 900 .
;;; h = [h2 > 4 ? (h2*300) : 0] + h3*900
(defun best-heuristic (e)
	(let ((h2 (h2-leveled-state e)))
		(+ (if (> h2 4) (* h2 300) 0) (* (h3-HOLES e) 900))))



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
          (frontier (make-binary-heap)))

		;Comeca por inserir o estado-inicial na frontier
        (min-heap-insert frontier (make-search-node :estado (problema-estado-inicial p) :lst-accoes (list NIL))
								  (funcall F (problema-estado-inicial p)))
        (loop
			;Faz pop da priority queue 
			(setf node (heap-pop frontier))

			;Caso a queue esteja vazia nao temos solucao
			(when (null node)
                (return-from best-first-search NIL))
            

            (cond ((eq (funcall (problema-solucao p) (search-node-estado node)) T) ;Caso tenhamos encontrado uma solucao
                (return-from best-first-search (cdr (search-node-lst-accoes node)))) ;cdr para remover o NIL inicialmente colocado
            (T  
                (let ((newE NIL) (prev-accoes (search-node-lst-accoes node)))
                    (dolist (a (funcall (problema-accoes p) (search-node-estado node)))
                        (setf newE (funcall (problema-resultado p) (search-node-estado node) a))
                        (min-heap-insert frontier (make-search-node :estado newE :lst-accoes (append prev-accoes (list a)))
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

;;; procura-uniforme: problema x (funcao: estado --> inteiro) --> lista-de-accoes
;;; Funcao que recebe um problema e uma funcao heuristica (a qual e ignorada) devolve uma lista de accoes
;;; corresponde ao caminho que resolve o problema recebido. Para orientar a funcao usa a funcao funcao de custo (g) (f = g).
(defun procura-uniforme (p h)
	(declare (ignore h))
    (best-first-search p #'(lambda(e) 
                            (funcall (problema-custo-caminho p) e))))

;;; procura-best: array x lista-pecas --> lista-de-accoes
;;; Funcao que recebe um array correspondente ao tabuleiro e uma lista de pecas por colocar. A funcao inicaliza um problema com 
;;; estes argumentos e devolve a melhor solucao, na forma de lista de accoes, dentro de um tempo aceitavel. Para tal usa a 
;;; heuristica 'best-heuristic' para guiar a procura.
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
        (procura-A* p1 #'best-heuristic)))

(load "utils.fas")
 