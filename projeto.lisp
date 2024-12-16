;;; Ficheiro: projeto.lisp
;;; Utiliza��o das funcionalidades do projeto
;;; Autores: Jo�o Fernandes e Rodrigo Santos

;; Inicializa��o

(load (merge-pathnames "puzzle.lisp" (current-pathname)))
(load (merge-pathnames "procura.lisp" (current-pathname)))
(load (merge-pathnames "desempenho.lisp" (current-pathname)))

(defun inicializar-problemas ()
  (escrever-problema "A" (problema-a))
  (escrever-problema "B" (problema-b))
  (escrever-problema "C" (problema-c))
  (escrever-problema "D" (problema-d))
  (escrever-problema "E" (problema-e))
  (escrever-problema "F" (problema-f))
  (escrever-problema "G" (problema-g))
)

;; Leitura utilizador

(defun iniciar ()
  (format t "Escolha um problema para resolver (de A a G): ")
  (let ((nome-problema (string-upcase (read-line))))
    (let ((resultado (obter-problema nome-problema)))
      (if resultado
          (progn
            (format t "Problema encontrado: ~A~%" nome-problema)
            (ler-algoritmo resultado))
          (format t "Problema ~A n�o encontrado.~%" nome-problema))))
)


(defun ler-algoritmo (problema)
  "Pede ao usu�rio para escolher um algoritmo e executa o algoritmo selecionado."
  (format t "Escolha o algoritmo a executar (BFS, DFS ou A-STAR): ")
  (force-output)
  (let ((input (string-upcase (read-line))))
    (cond
      ((string= input "BFS") 
       (format t "Executando BFS...~%")
       (bfs problema))
      ((string= input "DFS") 
       (format t "Digite a profundidade: ")
       (setf profundidade (read))
       (dfs problema profundidade))
      ((string= input "A-STAR") 
       (format t "Digite o nome da heur�stica: ")
       (setf heuristica (read))
       (a-star problema #'heuristica))
      (t 
       (format t "Op��o inv�lida.~%")))))

;; Escrita e leitura em ficheiros

(defun caminho-problemas ()
  "C:\\Users\\rodri\\Documents\\PROJETOS\\ia-projeto_1\\problemas.dat"
)

(defun escrever-problema (nome-problema tabuleiro-problema)
    (with-open-file (stream (caminho-problemas)
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
     (format stream "~A~%" nome-problema)
    (dolist (sublista tabuleiro-problema)
      (format stream "~{~A~^ ~}~%" sublista))
    )
    
    (format t "Problema escrito com sucesso ~%")
    T
)

(defun obter-problema (nome-problema)
  (with-open-file (stream (caminho-problemas) :direction :input)
    (obter-problema-stream stream nome-problema)))

(defun obter-problema-stream (stream nome-problema)
  (let ((linha (read-line stream nil)))
    (cond
      ((null linha) nil)
      ((string= linha nome-problema) 
       (let ((tabuleiro (ler-tabuleiro stream)))
         tabuleiro))
      (t (obter-problema-stream stream nome-problema)))))

(defun ler-tabuleiro (stream)
  (let ((linha1 (read-line stream nil))
        (linha2 (read-line stream nil)))
    (when (and linha1 linha2)
      (list 
       (read-from-string (concatenate 'string "(" linha1 ")"))
       (read-from-string (concatenate 'string "(" linha2 ")"))))))

(defun apresentar-desempenho (c)
  (format t "Penetr�ncia: ~A ~%" (penetrancia c))
  (format t "Fator Ramifica��o: ~A %" (fator-ramificacao (comprimento-caminho c) *numero-nos-gerados* -1 1 0.0001))
)
