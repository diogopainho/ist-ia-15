(load (compile-file "../../Users/Vasco/Google Drive/!!MINHAS PASTAS!!/2. Cadeiras/2015-2016 (1º sem)/IA/ist-ia-15/projeto.lisp"))
(load "../../Users/Vasco/Google Drive/!!MINHAS PASTAS!!/2. Cadeiras/2015-2016 (1º sem)/IA/ist-ia-15/utils.fas")

(defun clearTimer()
	(dotimes (i 1000000)
		i))
(defun test1()
	(test #'procura-A* '(i i i i) #'h2-leveled-state))

(defun test2()
	(test #'procura-A* '(o o o o o) #'h2-leveled-state))

(test1)
(test2)