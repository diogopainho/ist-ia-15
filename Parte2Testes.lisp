;;; Teste 12 E2 
;;; Testes funcao accoes em que o estado nao tem mais pecas por colocar ou esta cheio
;;deve retornar IGNORE
(ignore-value (setf t1 (cria-tabuleiro)))
;;deve retornar NIL
(accoes (make-estado :pontos 0 :pecas-por-colocar '() :pecas-colocadas '() :tabuleiro t1))
;;deve retornar uma lista de accoes para a peca l (ver ficheiro output)
(accoes (make-estado :pontos 0 :pecas-por-colocar '(l i j) :pecas-colocadas '() :tabuleiro t1))
;;deve retornar IGNORE
(ignore-value (dotimes (linha 18)(tabuleiro-preenche! t1 linha 0)))
;;deve retornar NIL
(accoes (make-estado :pontos 0 :pecas-por-colocar '(o t j) :pecas-colocadas '(i i i i) :tabuleiro t1))
;;deve retornar NIL
(accoes (make-estado :pontos 0 :pecas-por-colocar '(t l t) :pecas-colocadas '(i i i i) :tabuleiro t1))

;;; Teste 13 E2
;;; procura profundidade primeiro num tabuleiro vazio, e num tabuleiro onde nao existe solucao
;;deve retornar IGNORE
(ignore-value (setf t1 (cria-tabuleiro)))
;;deve retornar uma lista de accoes (ver ficheiro output)
(procura-pp (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(j l t o z s i)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'(lambda (x) 0)))
;;deve retornar IGNORE
(tabuleiro-preenche! t1 17 0)
;;deve retornar NIL (nao existe solucao)
(procura-pp (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(j l t o z s i)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'(lambda (x) 0)))

;;; Teste 14 E2 
;;; procura profundidade primeiro num tabuleiro com algumas pecas
;;deve retornar IGNORE
(ignore-value (setf t1 (cria-tabuleiro)))
(ignore-value (dotimes (coluna 9) (tabuleiro-preenche! t1 0 (+ coluna 1))(tabuleiro-preenche! t1 1 (+ coluna 1))(tabuleiro-preenche! t1 2 (+ coluna 1))))
;;deve retornar uma lista de accoes (ver ficheiro output)
(procura-pp (make-problema :estado-inicial (make-estado :pontos 0 :tabuleiro t1 :pecas-colocadas () :pecas-por-colocar '(o o o o o l l t t j j i i)) :solucao #'solucao :accoes #'accoes :resultado #'resultado :custo-caminho #'(lambda (x) 0)))