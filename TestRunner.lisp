(load (compile-file "../../Users/Vasco/Google Drive/!!MINHAS PASTAS!!/2. Cadeiras/2015-2016 (1º sem)/IA/ist-ia-15/projeto.lisp"))
(load "../../Users/Vasco/Google Drive/!!MINHAS PASTAS!!/2. Cadeiras/2015-2016 (1º sem)/IA/ist-ia-15/utils.fas")


(defconstant h #'(lambda (e) (+ (if (> (h2-leveled-state e) 4) (* (h2-leveled-state e) 300) 0) (* (h3-HOLES e) 900))))
;(defconstant h #'(lambda (e) (floor (+ (* (funcall #'h2-leveled-state e) 300) (* (funcall #'h3-HOLES e) 900)) 2)))
;(defconstant h #'(lambda (e) (* (funcall #'h3-HOLES e) 900)))
;(defconstant h #'(lambda (e) (* (funcall #'h2-leveled-state e) (* 80 (list-length (estado-pecas-por-colocar e))))))
;(defconstant h #'(lambda (e) (* (funcall #'h3-holes e) (* 80 (list-length (estado-pecas-por-colocar e))))))
;(defconstant h #'(lambda (e) (funcall #'h3-holes e)))

(defun test1()
	(test #'procura-A* (cria-tabuleiro) '(i i i i) h 100))

(defun test2()
	(test #'procura-A* (cria-tabuleiro) '(o o o o o) h 300))

(defun test3()
	(test #'procura-A* (cria-tabuleiro) '(i i o o o o) h 300))

(defun test4()
	(test #'procura-A* (cria-tabuleiro) '(o o o o i i) h 300))

(defun test5()
	(test #'procura-A* (cria-tabuleiro) '(i o o o o i) h 300))

(defun test6()
	(let ((tab (cria-tabuleiro)))
		(tabuleiro-preenche! tab 0 0) (tabuleiro-preenche! tab 0 1) (tabuleiro-preenche! tab 0 2) (tabuleiro-preenche! tab 0 3) (tabuleiro-preenche! tab 0 5)
		(tabuleiro-preenche! tab 0 8) (tabuleiro-preenche! tab 0 9) (tabuleiro-preenche! tab 1 0) (tabuleiro-preenche! tab 1 1) (tabuleiro-preenche! tab 1 5)
		(tabuleiro-preenche! tab 1 8) (tabuleiro-preenche! tab 1 9)
		(test #'procura-A* tab '(j o) h 300)))

(defun test7()
	(test #'procura-A* (cria-tabuleiro) '(i s o s l t) h 300))
