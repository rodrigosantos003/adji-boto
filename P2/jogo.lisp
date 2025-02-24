;;; Ficheiro: jogo.lisp
;;; Carregamento de ficheiros e intera��o com o utilizador
;;; Autores: Jo�o Fernandes e Rodrigo Santos

;; Inicializa��o

; Carregamento de m�dulos
(load (merge-pathnames "puzzle.lisp" (current-pathname)))
(load (merge-pathnames "algoritmo.lisp" (current-pathname)))

;; Leitura utilizador
(defun iniciar ()
  (format t "Escolha o modo de jogo (1-Humano vs Computador; 2-Computador vs Computador): ")
  (let ((modo (read)))
    (cond ((= modo 1) (humano-computador))
          ((= modo 2) (computador-computador))
          (t "Modo inv�lido"))))

(defun jogar (node tempo &optional (jogador 2))
  "Avalia os sucessores do n� usando NegaMax, respeitando um limite de tempo, e retorna o �ndice do melhor movimento e o novo n�."
  (let* ((depth 4)
         (color 1)
         (children (sucessores node jogador)) ;; Gera os sucessores do estado atual
         (tempo-inicio (get-internal-real-time)) ;; Marca o in�cio do c�lculo
         (tempo-limite tempo))
    (if children
        (progn
          (let* ((result (melhor-jogada-recursiva jogador children depth color 0 -1000 nil nil tempo-inicio tempo-limite))
                 (tempo-fim (get-internal-real-time)) ;; Marca o fim do c�lculo
                 (tempo-gasto (float (/ (- tempo-fim tempo-inicio) internal-time-units-per-second)))) ;; Calcula o tempo decorrido em milissegundos
            (apresentar-jogada result tempo-gasto) ;; Apresenta a jogada escolhida
            result))
        (list nil node)))) ;; Retorna nil se n�o houver sucessores

(defun melhor-jogada-recursiva (jogador sucessores depth color index melhor-score melhor-index melhor-node tempo-inicio tempo-limite)
  "Fun��o recursiva otimizada para encontrar a melhor jogada, com atualiza��o de alpha e limite de tempo."
  (if (or (null sucessores) ;; Sem mais sucessores para avaliar
          (> (- (get-internal-real-time) tempo-inicio) tempo-limite)) ;; Tempo limite atingido
      (progn
        (list (list (linha-jogador jogador) melhor-index) melhor-node)) ;; Retorna a melhor jogada encontrada at� agora
      ;; Caso contr�rio, avalia o pr�ximo sucessor
      (let ((sucessor-atual (car sucessores))) ;; Pega o sucessor atual
        (if (null sucessor-atual) ;; Se o sucessor for NIL, salta para o pr�ximo
            (melhor-jogada-recursiva jogador (cdr sucessores) depth color (1+ index) melhor-score melhor-index melhor-node tempo-inicio tempo-limite)
            ;; Caso contr�rio, avalia o sucessor
            (let ((score (negamax sucessor-atual depth jogador melhor-score 1000 color)))
                  ;; Atualiza melhor jogada
                  (melhor-jogada-recursiva 
                   jogador 
                   (cdr sucessores) 
                   depth 
                   color 
                   (1+ index) 
                   (max score melhor-score) 
                   (if (> score melhor-score) index melhor-index) 
                   (if (> score melhor-score) sucessor-atual melhor-node) 
                   tempo-inicio tempo-limite))))))

;; Humano vs Computador
(defun humano-computador ()
  (format t "Quer iniciar primeiro? (1-Sim; 2-N�o): ")
  (let* ((inicio (read))
         (jogador-humano (if (= inicio 1) 1 2))
         (jogador-computador (alternar-jogador jogador-humano))
         (tempo-limite (progn
                         (format t "Insira o limite de tempo para o computador (em milissegundos): ")
                         (read))))
    (jogar-humano-computador (tabuleiro-inicial) jogador-humano jogador-computador (if (= jogador-computador 1) t nil) tempo-limite)))


(defun jogar-humano (node jogador-humano)
  "Gerencia a jogada do jogador humano."
  ;; Verifica se h� jogadas dispon�veis
  (format t "TABULEIRO ATUAL:~%")
  (apresentar-tabuleiro (estado node) t)
  (format t "--------------------------------------~%")

  (format t "Turno do jogador humano (~A).~%" jogador-humano)
  (if (linha-vaziap (nth (linha-jogador jogador-humano) (estado node)))
      (progn
        (format t "N�o h� mais jogadas dispon�veis para o jogador humano (~A).~%" jogador-humano)
        node) ;; Indica que n�o h� jogada poss�vel
    (progn
      (let* ((inicio-tempo (get-internal-real-time))
             (jogada-humano (ler-jogada-humano node jogador-humano))
             (tempo-gasto (float (/ (- (get-internal-real-time) inicio-tempo) internal-time-units-per-second))))
        ;; Apresenta a jogada e o tempo gasto
        (apresentar-jogada jogada-humano tempo-gasto)
        (jogada-node jogada-humano))))) ;; Retorna o novo estado do n�

(defun jogar-computador (node jogador-computador tempo)
  "Gerencia a jogada do computador com um limite de tempo."
  (format t "TABULEIRO ATUAL:~%")
  (apresentar-tabuleiro (estado node) t)
  (format t "--------------------------------------~%")

  (format t "Turno do jogador computador (~A).~%" jogador-computador)
  ;; Verifica se h� jogadas dispon�veis
  (if (linha-vaziap (nth (linha-jogador jogador-computador) (estado node)))
      (progn
        (format t "N�o h� mais jogadas dispon�veis para o jogador computador (~A).~%" jogador-computador)
        node) ;; Indica que n�o h� jogada poss�vel
      (progn
        (let ((jogada-computador (jogar node tempo jogador-computador)))
          (jogada-node jogada-computador))))) ;; Retorna o novo estado do n�

(defun jogar-humano-computador (node jogador-humano jogador-computador computador-primeiro tempo)
  "Gerencia as jogadas entre o jogador humano e o computador.
   Se `computador-primeiro` for verdadeiro, o computador joga primeiro."
  (when computador-primeiro
    ;; Jogada inicial do computador, se for o primeiro
    (setf node (jogar-computador node jogador-computador tempo))
    (unless node
      (return-from jogar-humano-computador))) ;; Encerra se o jogo terminar ap�s a jogada inicial do computador

  (loop
     ;; Jogada do jogador humano
    (setf node (jogar-humano node jogador-humano))

    (if (jogo-terminado-p node)
        (progn
          (if (= (maximo-pontos node) jogador-humano) (format t "O jogador humano (~A) venceu!" jogador-humano)
            (format t "O jogador computador (~A) venceu!" jogador-computador))
          (return-from jogar-humano-computador)))

    ;; Jogada do computador
    (setf node (jogar-computador node jogador-computador tempo))

    (if (jogo-terminado-p node)
        (progn
          (if (= (maximo-pontos node) jogador-humano) (format t "O jogador humano (~A) venceu!" jogador-humano)
            (format t "O jogador computador (~A) venceu!" jogador-computador))
          (return-from jogar-humano-computador)))))


(defun ler-jogada-humano (node jogador)
  "L� a jogada do jogador humano, valida se a coluna tem pe�as dispon�veis,
   atualiza o estado do n� e retorna o novo n�."
  (let ((jogada-valida nil)) ;; Vari�vel para armazenar a jogada v�lida
    ;; Loop para garantir que a jogada � v�lida
    (loop
      (format t "Indique a coluna onde pretende jogar (entre 1 e 6): ")
      (let ((coluna (read))) ;; L� a coluna escolhida pelo jogador
        ;; Verifica se a coluna � v�lida usando `celula-distribuivelp`
        (if (and (integerp coluna) ;; Garante que � um n�mero inteiro
                 (>= coluna 1) (<= coluna 6) ;; Garante que est� no intervalo permitido
                 (celula-distribuivelp (linha-jogador jogador) (- coluna 1) (estado node)))
            (progn
              (setf jogada-valida coluna) ;; Armazena a jogada v�lida
              (return)) ;; Sai do loop
          (format t "A coluna escolhida n�o � v�lida ou est� vazia. Por favor, escolha outra.~%"))))
    ;; Retorna o novo n� atualizado com a jogada v�lida
    (list (list (linha-jogador jogador) jogada-valida)
          (gerar-node node (aplicar-operador (estado node) (linha-jogador jogador) (- jogada-valida 1)) jogador))))


(defun jogo-terminado-p (node)
  "Verifica se o jogo terminou."
  (tabuleiro-vaziop (estado node)))

(defun maximo-pontos (node)
  (if (> (pontuacao-1 node) (pontuacao-2 node)) 1 2)
)

;; Computador vs Computador
(defun computador-computador ()
    (jogar-computador-computador (tabuleiro-inicial) 5000))

(defun jogar-computador-computador (node tempo)
  (loop
     ;; Jogada do computador 1
     (setf node (jogar-computador node 1 tempo))
     (when (jogo-terminado-p node)
       (progn
         (format t "O jogador computador (~A) venceu!~%" (maximo-pontos node))
         (return)))

     ;; Jogada do computador 2
     (setf node (jogar-computador node 2 tempo))
     (when (jogo-terminado-p node)
       (progn
         (format t "O jogador computador (~A) venceu!~%" (maximo-pontos node))
         (return)))))

;; Escrita em ficheiros
(defun caminho-logs ()
  "./log.dat"
)

(defun escrever-jogada (jogada tempo stream)
  (format stream "Jogada ~A:~%" (jogada-posicao jogada))
  (apresentar-tabuleiro (jogada-estado jogada) stream)
  (format stream "Pontua��o: [Jogador 1 - ~A] | [Jogador 2 - ~A] ~%" (pontuacao-1 (jogada-node jogada)) (pontuacao-2 (jogada-node jogada)))
  (format stream "Nos analisados: ~A | Cortes: ~A | Tempo gasto (s): ~A~%" *numero-nos-analisados* *numero-cortes* tempo)
  (format stream "--------------------------------------~%~%~%"))

(defun apresentar-jogada (jogada tempo)
  (escrever-jogada jogada tempo t)
  (with-open-file (stream (caminho-logs):direction :output :if-exists :append :if-does-not-exist :create) (escrever-jogada jogada tempo stream))
  (repor-contagem)
)

(defun apresentar-tabuleiro (tabuleiro stream)
(dolist (sublista tabuleiro)
    (format stream "~{~A~^ ~}~%" sublista)))

(defun alternar-jogador (jogador)
  (if (= jogador 1) 2 1)
)

(defun jogada-posicao (jogada)
  (first jogada)
)

(defun jogada-estado (jogada)
  (estado (jogada-node jogada))
)

(defun jogada-node (jogada)
  (second jogada)
)

(defun linha-jogador (jogador)
  (- jogador 1))