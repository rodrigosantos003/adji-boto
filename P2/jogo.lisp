;;; Ficheiro: jogo.lisp
;;; Carregamento de ficheiros e interação com o utilizador
;;; Autores: João Fernandes e Rodrigo Santos

;; Inicialização

; Carregamento de módulos
(load (merge-pathnames "puzzle.lisp" (current-pathname)))
(load (merge-pathnames "algoritmo.lisp" (current-pathname)))

;; Leitura utilizador

(defun iniciar ()
  "Inicia o programa para o utilizador e solicita qual o modo a jogar"
  (repor-contar-nos)
  (format t "Escolha um modo de jogo (1-Humano vs Computador; 2-Computador vs Computador): ")
  (let ((nome-problema (string-upcase (read-line))))
    (let ((resultado (obter-problema nome-problema)))
      (if resultado
          (progn
            (format t "Problema encontrado: ~A~%" nome-problema)
            (ler-algoritmo resultado))
          (format t "Problema ~A não encontrado.~%" nome-problema))))
)

(defun ler-algoritmo (problema)
  "Solicita ao utilizador um algoritmo e os respetivos argumentos"
  (format t "Escolha o algoritmo a executar (BFS, DFS ou A-STAR): ")
  (force-output)
  (let ((input (string-upcase (read-line))))
    (cond
      ((string= input "BFS") 
       (format t "Executando BFS...~%")
       (executar-bfs problema))
      ((string= input "DFS") 
       (format t "Digite a profundidade (-1 se não quiser dar profundidade): ")
       (let ((profundidade (read)))
         (if (= profundidade -1)
             (executar-dfs problema)
             (executar-dfs problema profundidade))))
      ((string= input "A-STAR") 
       (format t "Indique a heurística (1- base; 2- personalizada): ")
       (let ((heuristica (read)))
         (cond ((= heuristica 1) (executar-a-star problema #'heuristicaBase))
               ((= heuristica 2) (executar-a-star problema #'heuristicaPersonalizada))
               (t (format t "Opção inválida. ~%")))))
      (t 
       (format t "Opção inválida.~%")))))


;; Escrita e leitura em ficheiros

(defun caminho-problemas ()
  "./log.dat"
)

(defun escrever-problema (nome-problema tabuleiro-problema)
  "Escreve um problema no ficheiro de problemas"
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
  "Obtém um problema do ficheiro de problemas"
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
  "Lê um tabuleiro de uma stream"
  (let ((linha1 (read-line stream nil))
        (linha2 (read-line stream nil)))
    (when (and linha1 linha2)
      (list 
       (read-from-string (concatenate 'string "(" linha1 ")"))
       (read-from-string (concatenate 'string "(" linha2 ")"))))))