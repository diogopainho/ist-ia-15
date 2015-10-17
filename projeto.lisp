;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Grupo 38 - Alameda - 72471 Michael Santos - 73245 Diogo Painho - 75219 Joao Franco ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load utils.lisp)

;;;;;;;;;;;;;;;;;;;;
;;;; TIPO ACCAO ;;;;
;;;;;;;;;;;;;;;;;;;;
(defstruct (accao (:constructor cria-accao (inteiro array)))
  inteiro
  array)

(defun accao-coluna (array)
  accao-inteiro array)

(defun accao-peca (accao))

;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO TABULEIRO ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun cria-tabuleiro ())

(defun copia-tabuleiro (tabuleiro))

(defun tabuleiro-preenchido-p (tabuleiro inteiro1 inteiro2))

(defun tabuleiro-altura-coluna (tabuleiro inteiro))

(defun tabuleiro-linha-completa-p (tabuleiro inteiro))

(defun tabuleiro-preenche! (tabuleiro inteiro1 inteiro2))

(defun tabuleiro-remove-linha! (tabuleiro inteiro))

(defun tabuleiro-topo-preenchido-p (tabuleiro))

(defun tabuleiros-iguais-p (tabuleiro1 tabuleiro2))

(defun tabuleiro->array (tabuleiro))

(defun array->tabuleiro (array))


;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO ESTADO ;;;;
;;;;;;;;;;;;;;;;;;;;;

(defun copia-estado (estado))

(defun estados-iguais-p (estado1 estado2))

(defun estado-final-p (estado))

;;;;;;;;;;;;;;;;;;;;;;;
;;;; TIPO PROBLEMA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;
(defstruct problema estado-inicial solucao accoes resultado custo-caminho)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; FUNCOES DO PROBLEMA DE PROCURA ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solucao (estado))

(defun accoes (estado))

(defun resultado (estado accao))

(defun qualidade (estado))

(defun custo-oportunidade (estado))

;;;;;;;;;;;;;;;;;
;;;; PROCURA ;;;;
;;;;;;;;;;;;;;;;;

(defun procura-pp (problema))

(defun procura-A* (problema heuristica))

(defun procura-best (array lista-pecas))
