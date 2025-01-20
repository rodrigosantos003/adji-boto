;;; Ficheiro: jogo.lisp
;;; Carregamento de ficheiros e interação com o utilizador
;;; Autores: João Fernandes e Rodrigo Santos

;; Inicialização

; Carregamento de módulos
(load (merge-pathnames "puzzle.lisp" (current-pathname)))
(load (merge-pathnames "algoritmo.lisp" (current-pathname)))

;; Leitura utilizador
(defun iniciar ()
  (format t "Escolha o modo de jogo (1-Humano vs Computador; 2-Computador vs Computador): ")
  (let ((modo (read)))
    (cond ((= modo 1) (humano-computador))
          ((= modo 2) (computador-computador))
          (t "Modo inválido"))))

(defun jogar (node jogador)
  "Avalia os sucessores do nó usando NegaMax e retorna o índice do melhor movimento e o novo nó."
  (let* ((depth 4)
         (color 1)
         (children (sucessores node jogador))
         (start-time (get-internal-real-time)))
    (if children
        (progn
          (let* ((result (melhor-jogada-recursiva jogador children depth color 0 most-negative-fixnum nil nil))
                 (end-time (get-internal-real-time))
                 (time-spent (float (/ (- end-time start-time) internal-time-units-per-second))))
            (apresentar-jogada result time-spent)
            result))
        (list nil node)))) ; Retorna nil como índice se não houver sucessores

(defun melhor-jogada-recursiva (jogador sucessores depth color index melhor-score melhor-index melhor-node)
  "Função recursiva auxiliar para encontrar a melhor jogada."
  (if (null sucessores)
      (list melhor-index melhor-node)
      (let* ((current-node (car sucessores))
             (score (negamax current-node depth jogador most-negative-fixnum most-positive-fixnum color)))
        (if (> score melhor-score)
            (melhor-jogada-recursiva jogador (cdr sucessores) depth color (1+ index) score index current-node)
            (melhor-jogada-recursiva jogador (cdr sucessores) depth color (1+ index) melhor-score melhor-index melhor-node)))))


;; Humano vs Computador
(defun humano-computador ()
  (format t "Quer iniciar primeiro? (1-Sim; 2-Não): ")
  (let* ((inicio (read))
         (jogador-humano (if (= inicio 1) 1 2))
         (jogador-computador (alternar-jogador jogador-humano)))
    (jogar-humano-computador (tabuleiro-inicial) jogador-humano jogador-computador)))

(defun jogar-humano-computador (node jogador-humano jogador-computador computador-inicial)
  "Gerencia as jogadas entre o jogador humano e o computador."
  (loop
    ;; Verifica se o jogador humano pode jogar
    (when (linha-vaziap (nth (linha-jogador jogador-humano) (estado node)))
      (format t "Não há mais jogadas disponíveis para o jogador humano.~%")
      (return-from jogar-humano-computador))

    ;; Jogada do jogador humano
    (format t "~%~%Turno do jogador humano.~%")

    ;; Inicia o temporizador para a jogada do humano
    (let ((inicio-tempo (get-internal-real-time)))
      ;; Atualiza o nó com a jogada do jogador humano e obtém o novo estado
      (let ((jogada-humano (ler-jogada-humano node jogador-humano)))
        ;; Calcula o tempo gasto na jogada
        (let ((tempo-gasto (/ (- (get-internal-real-time) inicio-tempo)
                               internal-time-units-per-second))) ; em segundos
          ;; Apresentar a jogada com o tempo gasto
          (apresentar-jogada jogada-humano tempo-gasto))

      ;; Verificar se o jogo terminou após a jogada do humano
      (when (jogo-terminado-p (jogada-node jogada-humano))
        (format t "O jogador humano venceu!~%")
        (return-from jogar-humano-computador))

      (setf node (jogada-node jogada-humano))

      (when (linha-vaziap (nth (linha-jogador jogador-computador) (estado node)))
      (format t "Não há mais jogadas disponíveis para o jogador computador.~%")
      (return-from jogar-humano-computador))

    ;; Jogada do computador
    (format t "~%~%Turno do computador.~%")
    (let* ((jogada-computador (jogar node jogador-computador)))

      ;; Verificar se o jogo terminou após a jogada do computador
      (when (jogo-terminado-p (jogada-node jogada-computador))
        (format t "O computador venceu!~%")
        (return-from jogar-humano-computador))

     (setf node (jogada-node jogada-computador)))))))






(defun ler-jogada-humano (node jogador)
  "Lê a jogada do jogador humano, atualiza o estado do nó e retorna o novo nó."
  (format t "Indique a coluna onde pretende jogar (entre 1 e 6): ")
  (let ((coluna (read)))
      ;; Retornar o novo nó atualizado
      (list coluna (gerar-node node (aplicar-operador (estado node) (linha-jogador jogador) (- coluna 1)) jogador))))

(defun jogo-terminado-p (node)
  "Verifica se o jogo terminou."
  (tabuleiro-vaziop (estado node)))

;; Computador vs Computador
(defun computador-computador ()
  (format t "Indique o tempo limite para os computadores (em milissegundos): ")
  (let* ((tempo (read))
         (jogador1 1)
         (jogador2 -1))
    (format t "Iniciando jogo Computador vs Computador...~%")
    (format t "Iniciar jogo")))

;; Escrita em ficheiros
(defun caminho-logs ()
  "./log.dat"
)

(defun escrever-jogada (jogada tempo stream)
  (format stream "Jogada (coluna ~A):~%" (jogada-coluna jogada))
  (dolist (sublista (jogada-estado jogada))
    (format stream "~{~A~^ ~}~%" sublista))
    (format stream "Pontuação: [Jogador 1 - ~A] | [Jogador 2 - ~A] ~%" (pontuacao-1 (jogada-node jogada)) (pontuacao-2 (jogada-node jogada)))
    (format stream "Nos analisados: ~A | Cortes: ~A | Tempo gasto (s): ~A~%" *numero-nos-analisados* *numero-cortes* tempo))

(defun apresentar-jogada (jogada tempo)
  (escrever-jogada jogada tempo t)
  (with-open-file (stream (caminho-logs):direction :output :if-exists :append :if-does-not-exist :create) (escrever-jogada jogada tempo stream))
  (repor-contagem)
)

(defun alternar-jogador (jogador)
  (if (= jogador 1) 2 1)
)

(defun jogada-coluna (jogada)
  (first jogada)
)

(defun jogada-estado (jogada)
  (estado (jogada-node jogada))
)

(defun jogada-node (jogada)
  (second jogada)
)