(in-package :p55)

(defvar *negamax-cache* (make-hash-table :test 'equal))

(defun jogar (node tempo jogador)
  "Avalia os sucessores do nó usando NegaMax, respeitando um limite de tempo, e retorna o índice do melhor movimento e o novo nó."
  (let* ((depth 9)
         (color 1)
         (children (sucessores node jogador)) ;; Gera os sucessores do estado atual
         (tempo-inicio (get-internal-real-time)) ;; Marca o início do cálculo
         (tempo-limite tempo))
    (if children
        (progn
          (let* ((result (melhor-jogada-recursiva jogador children depth color 0 -1000 nil nil tempo-inicio tempo-limite)))
            result))
        (list nil node)))) ;; Retorna nil se não houver sucessores

(defun melhor-jogada-recursiva (jogador sucessores depth color index melhor-score melhor-index melhor-node tempo-inicio tempo-limite)
  "Função recursiva otimizada para encontrar a melhor jogada, com atualização de alpha e limite de tempo."
  (if (or (null sucessores) ;; Sem mais sucessores para avaliar
          (> (- (get-internal-real-time) tempo-inicio) tempo-limite)) ;; Tempo limite atingido
      (progn
        (list (list (linha-jogador jogador) melhor-index) melhor-node)) ;; Retorna a melhor jogada encontrada até agora
      ;; Caso contrário, avalia o próximo sucessor
      (let ((sucessor-atual (car sucessores))) ;; Pega o sucessor atual
        (if (null sucessor-atual) ;; Se o sucessor for NIL, salta para o próximo
            (melhor-jogada-recursiva jogador (cdr sucessores) depth color (1+ index) melhor-score melhor-index melhor-node tempo-inicio tempo-limite)
            ;; Caso contrário, avalia o sucessor
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

(defun negamax (node depth jogador &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (color 1))
  "Implementa o algoritmo Negamax com memorização e poda alfa-beta."
  (let ((cache-key (list node depth jogador alpha beta color)))
    (or (gethash cache-key *negamax-cache*)
        (setf (gethash cache-key *negamax-cache*)
              (if (or (zerop depth) (terminalp node))
                  (* color (evaluate node jogador))
                (labels ((negamax-recursivo (children alpha beta)
                           (if (null children)
                               alpha
                             (progn
                               (let* ((new-alpha (max alpha (- (negamax (car children)
                                                                 (1- depth)
                                                                 (alternar-jogador jogador)
                                                                 (- beta)
                                                                 (- alpha)
                                                                 (- color))))))
                                 (if (>= new-alpha beta)
                                     (progn
                                       beta)
                                   (negamax-recursivo (cdr children) new-alpha beta)))))))
                  (negamax-recursivo (or (remove nil (sucessores node jogador)) (list node)) alpha beta)))))))

(defun terminalp (node)
  (tabuleiro-vaziop (estado node)))

(defun evaluate (node jogador)
    (if (= jogador 1) 
        (- (pontuacao-1 node) (pontuacao-2 node))
        (- (pontuacao-2 node) (pontuacao-1 node))))

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

(defun alternar-jogador (jogador)
  (if (= jogador 1) 2 1)
)

(defun sucessores (node jogador)
(mapcar (lambda (op)
              (when op
                (gerar-node node (funcall op (estado node)) jogador)))
            (gerar-operadores (estado node) (linha-jogador jogador))))

(defun gerar-node (nodeAntigo estadoNovo jogador)
  (let ((pecas-capturadas (- (soma-pecas (estado nodeAntigo)) (soma-pecas estadoNovo))))
    (if (= jogador 1)
        (list estadoNovo (list (+ (pontuacao-1 nodeAntigo) pecas-capturadas) (pontuacao-2 nodeAntigo)))
        (list estadoNovo (list (pontuacao-1 nodeAntigo) (+ (pontuacao-2 nodeAntigo) pecas-capturadas))))))

;;; Seletores

(defun linha (l tabuleiro)
  (cond ((or (not (integerp l)) (< l 0) (> l 1))
         (error "A linha só pode ser 0 ou 1, mas recebeu ~A" l))
        (t (nth l tabuleiro)))
)

(defun celula (l c tabuleiro)
  (cond ((or (< c 0) (> c 5)) (error "A coluna tem que estar entre 0 e 5"))
        (t (nth c (linha l tabuleiro))))
)

(defun estado (node)
  (first node))

(defun pontuacao-1 (node)
  (first (second node)))

(defun pontuacao-2 (node)
  (second (second node)))

;;; Funções auxiliares

(defun tabuleiro-inicial()
 '(((8 8 8 8 8 8)(8 8 8 8 8 8)) (0 0))
)

(defun tabuleiro-vaziop (tabuleiro)
  "Verifica se o tabuleiro está vazio"
  (every (lambda (linha) (every #'zerop linha)) tabuleiro)
)

(defun linha-vaziap (linha)
  (every #'zerop linha)
)

(defun substituir-posicao (idx lista &optional (valor 0))
  (cond ((zerop idx) (cons valor (cdr lista)))
    (t (cons (car lista)
             (substituir-posicao (1- idx) (cdr lista) valor)))))

(defun substituir (l c tabuleiro &optional (valor 0))
  "Substitui o valor na posição (l, c) do tabuleiro pelo 'valor'."
  (cond
    ((zerop l) (cons (substituir-posicao c (car tabuleiro) valor) (cdr tabuleiro)))
    (t (cons (car tabuleiro) (substituir (1- l) c (cdr tabuleiro) valor))))
)

(defun incrementar-posicao (l c tabuleiro)
  (substituir l c tabuleiro (1+ (celula l c tabuleiro)))
)

;;; Operadores
(defun distribuir-pecas (num-pecas linha col &optional (linha-inicial linha) (col-inicial col))
  "Retorna uma lista com os pares de índices onde serão colocadas as peças, no sentido anti-horário."
  (let* ((num-linhas (length (estado (tabuleiro-inicial))))
         (num-colunas (length (first (estado (tabuleiro-inicial))))))
    (cond
      ((<= num-pecas 0) '()) 
      (t
       (let* ((proxima-linha 
               (cond 
                ((and (= linha 0) (> col 0)) linha) ; parte superior
                ((and (= col 0) (< linha (1- num-linhas))) (1+ linha)) ; lado esquerdo
                ((and (= linha (1- num-linhas)) (< col (1- num-colunas))) linha) ; parte inferior
                ((and (= col (1- num-colunas)) (> linha 0)) (1- linha)))) ; lado direito
              (proxima-col
               (cond 
                ((and (= linha 0) (> col 0)) (1- col)) ; parte superior
                ((and (= col 0) (< linha (1- num-linhas))) col) ; lado esquerdo
                ((and (= linha (1- num-linhas)) (< col (1- num-colunas))) (1+ col)) ; parte inferior
                ((and (= col (1- num-colunas)) (> linha 0)) col)))) ; lado direito

         (cond ((and (= proxima-linha linha-inicial) (= proxima-col col-inicial)) (distribuir-pecas num-pecas proxima-linha proxima-col linha-inicial col-inicial))
               (t (cons (list proxima-linha proxima-col)
               (distribuir-pecas (1- num-pecas) proxima-linha proxima-col linha-inicial col-inicial)))))))))


(defun aplicar-operador (tabuleiro linha-idx col-idx)
  "Aplica o operador às peças do buraco especificado."
  (let* ((pecas-a-retirar (celula linha-idx col-idx tabuleiro))
         (tabuleiro-sem-pecas (substituir linha-idx col-idx tabuleiro 0))
         (posicoes-distribuicao (distribuir-pecas pecas-a-retirar linha-idx col-idx))
         (ultima-posicao (car (last posicoes-distribuicao)))
         (ultima-linha (first ultima-posicao))
         (ultima-coluna (second ultima-posicao))
         (novo-tabuleiro (incrementar-posicoes posicoes-distribuicao tabuleiro-sem-pecas))
         (pecas-na-ultima (celula ultima-linha ultima-coluna novo-tabuleiro)))
    ;; Verifica se as peças na última posição são 1, 3 ou 5 para retirar as peças, senão retorna o tabuleiro com as peças distribuídas
    (let ((tabuleiro-final
           (if (and (/= ultima-linha linha-idx) (member pecas-na-ultima '(1 3 5)))  ; Corrigido: removidos os parênteses excessivos
               (substituir ultima-linha ultima-coluna novo-tabuleiro 0)  ; Zera a última célula
               novo-tabuleiro)))  ; Caso contrário, retorna o tabuleiro com as peças distribuídas normalmente
      tabuleiro-final)))


(defun incrementar-posicoes (posicoes tabuleiro)
  (if (null posicoes) ; Caso base: lista de posições vazia
      tabuleiro
      (let ((l (first (first posicoes)))  ; Linha
            (c (second (first posicoes)))) ; Coluna
        (incrementar-posicoes (rest posicoes) ; Chamada recursiva com a cauda da lista
                               (incrementar-posicao l c tabuleiro))))) ; Incrementa a célula

(defun gerar-operadores (tabuleiro linha &optional (coluna 0) (resultados '()))
  (if (>= coluna (length (first tabuleiro))) ; Verifica se todas as colunas foram percorridas.
      (reverse resultados)                   ; Retorna os resultados em ordem.
      (let ((nova-coluna (if (>= coluna (length (nth linha tabuleiro)))
                             0
                             (1+ coluna))))
        (gerar-operadores tabuleiro linha nova-coluna
         (cons (if (and (< linha (length tabuleiro)) ; Verifica se ainda está dentro da matriz.
                        (< coluna (length (nth linha tabuleiro)))
                        (celula-distribuivelp linha coluna tabuleiro)) ; Condição customizável.
                   (lambda (tabuleiro) (aplicar-operador tabuleiro linha coluna)) ; Lambda para células válidas.
                   nil)
               resultados)))))


(defun celula-distribuivelp (linha coluna matriz)
  "Retorna T se a célula tem peças suficientes para uma distribuição válida."
  (let ((valor (celula linha coluna matriz)))
    (and (> valor 0) t)))

(defun soma-pecas (tabuleiro)
 (+ (apply '+ (first tabuleiro)) (apply '+ (second tabuleiro)))
)

(defun pecas-eliminadas (linha coluna tabuleiro)
  (let* ((ultima-posicao (first (last (distribuir-pecas (celula linha coluna tabuleiro) linha coluna tabuleiro))))
         (ultima-posicao-pecas (celula (first ultima-posicao) (second ultima-posicao) tabuleiro))
         (num-pecas (+ (ceiling (celula linha coluna tabuleiro) 12) ultima-posicao-pecas)))
    (cond
      ((= num-pecas 1) 1)
      ((= num-pecas 3) 3)
      ((= num-pecas 5) 5)
      (t 0))))


(defun verificar-pecas-eliminadas (tabuleiro &optional (linha 0) (coluna 0) (resultados '()))
  (if (>= linha (length tabuleiro)) ; Verifica se todas as linhas foram percorridas.
      (let* ((resultados-sem-zero (remove-if-not #'(lambda (x) (> x 0)) resultados)) ; Filtra os 0.
             (total (apply '+ resultados-sem-zero)) ; Soma dos resultados não nulos.
             (quantidade (length resultados-sem-zero))) ; Quantidade de elementos não nulos.
        (if (> quantidade 0) ; Verifica se há elementos para calcular a média.
            (floor (/ total quantidade)) ; Calcula a média.
            0)) ; Se não houver elementos não nulos, retorna 0.
    (let ((nova-linha (if (>= coluna (length (nth linha tabuleiro)))
                          (1+ linha)
                        linha))
          (nova-coluna (if (>= coluna (length (nth linha tabuleiro)))
                           0
                         (1+ coluna))))
      (if (and (< linha (length tabuleiro)) ; Verifica se ainda está dentro da matriz.
               (< coluna (length (nth linha tabuleiro)))
               (celula-distribuivelp linha coluna tabuleiro)) ; Condição customizável.
          (verificar-pecas-eliminadas tabuleiro nova-linha nova-coluna
           (cons (pecas-eliminadas linha coluna tabuleiro) resultados)) ; Adiciona o lambda à lista.
        (verificar-pecas-eliminadas tabuleiro nova-linha nova-coluna resultados)))))