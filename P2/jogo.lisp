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

(defun jogar (node)
  "Avalia os sucessores do nó usando NegaMax e retorna o índice do melhor movimento e o novo nó."
  (let* ((depth 3)
         (color 1)
         (children (sucessores node color))
         (start-time (get-internal-real-time)))
    (if children
        (progn
          (let* ((result (melhor-jogada-recursiva children depth color 0 most-negative-fixnum nil nil))
                 (end-time (get-internal-real-time))
                 (time-spent (float (/ (- end-time start-time) internal-time-units-per-second))))
            (apresentar-jogada node time-spent)
            result))
        (list nil node)))) ; Retorna nil como índice se não houver sucessores

(defun melhor-jogada-recursiva (sucessores depth color index melhor-score melhor-index melhor-node)
  "Função recursiva auxiliar para encontrar a melhor jogada."
  (if (null sucessores)
      (list melhor-index melhor-node)
      (let* ((current-node (car sucessores))
             (score (negamax current-node depth most-negative-fixnum most-positive-fixnum color)))
        (if (> score melhor-score)
            (melhor-jogada-recursiva (cdr sucessores) depth color (1+ index) score index current-node)
            (melhor-jogada-recursiva (cdr sucessores) depth color (1+ index) melhor-score melhor-index melhor-node)))))


;; Humano vs Computador
(defun humano-computador ()
  (format t "Quer iniciar primeiro? (1-Sim; 2-Não): ")
  (let* ((inicio (read))
         (jogador-humano (if (= inicio 1) 1 -1))
         (jogador-computador (- jogador-humano)))
    (jogar-humano-computador (tabuleiro-inicial) jogador-humano jogador-computador)))

(defun jogar-humano-computador (node jogador-humano jogador-computador)
  "Gerencia as jogadas entre o jogador humano e o computador."
  ;; Jogada do jogador humano
  (format t "Turno do jogador humano.~%")
  
  ;; Inicia o temporizador para a jogada do humano
  (let ((inicio-tempo (get-internal-real-time)))
    ;; Atualiza o nó com a jogada do jogador humano e obtém o novo estado
    (let ((novo-node (ler-jogada-humano node jogador-humano)))
      ;; Calcula o tempo gasto na jogada
      (let ((tempo-gasto (/ (- (get-internal-real-time) inicio-tempo)
                             internal-time-units-per-second))) ; em segundos
        ;; Apresentar a jogada com o tempo gasto
        (apresentar-jogada novo-node tempo-gasto))

      ;; Verificar se o jogo terminou após a jogada do humano
      (when (jogo-terminado-p novo-node)
        (format t "O jogador humano venceu!~%")
        (return-from jogar-humano-computador))

      ;; Jogada do computador
      (format t "Turno do computador.~%")
      (let* ((result (jogar novo-node))
             (novo-node-computador (second result)))
        ;; Apresentar a jogada do computador
        (apresentar-jogada novo-node-computador 0) ; Preserva o zero para o computador

        ;; Verificar se o jogo terminou após a jogada do computador
        (when (jogo-terminado-p novo-node-computador)
          (format t "O computador venceu!~%")
          (return-from jogar-humano-computador))

        ;; Chama recursivamente para o próximo turno
        (jogar-humano-computador novo-node-computador jogador-humano jogador-computador)))))


(defun ler-jogada-humano (node jogador)
  "Lê a jogada do jogador humano, atualiza o estado do nó e retorna o novo nó."
  (format t "Indique a coluna onde pretende jogar (entre 1 e 6): ")
  (let ((coluna (read)))
      ;; Retornar o novo nó atualizado
      (gerar-node node (aplicar-operador (estado node) (linha-jogador jogador) coluna) jogador)))

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

(defun escrever-jogada (node tempo stream)
  (format stream "Jogada:~%")
  (dolist (sublista (estado node))
    (format stream "~{~A~^ ~}~%" sublista))
    (format stream "Nos analisados: ~A | Cortes: ~A | Tempo gasto (s): ~A~%" *numero-nos-analisados* *numero-cortes* tempo))

(defun apresentar-jogada (node tempo)
  (escrever-jogada node tempo t)
  (with-open-file (stream (caminho-logs):direction :output :if-exists :append :if-does-not-exist :create) (escrever-jogada node tempo stream))
  (repor-contagem)
)