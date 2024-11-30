;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e João Fernandes

;; Travessias
(defun bfs (tabuleiro)
  "Pesquisa em largura"
  (let ((abertos (list (list nil nil tabuleiro 0)))  ; Inicializa a lista de abertos
        (fechados '()))                           ; Inicializa a lista de fechados
    (loop
       (if (null abertos)  ; Se a lista de abertos estiver vazia, termine a busca
           (return nil))   ; Não encontrou o objetivo
      (let ((node (first abertos)))  ; Pega o primeiro nó da lista de abertos
        (setq abertos (rest abertos))  ; Remove o nó atual da lista de abertos
        (push node fechados)  ; Adiciona o nó atual à lista de fechados
        (if (tabuleiro-vaziop (third node))  ; Verifica se é o objetivo
            (return (caminho node fechados)))  ; Retorna o caminho completo até o objetivo
        (let ((sucessores (gerar-filhos node)))  ; Gera sucessores
          (dolist (s sucessores)  ; Itera sobre cada sucessor
            (unless (or (member (third s) (mapcar #'third abertos) :test #'equal)  ; Verifica se está em abertos
                        (member (third s) (mapcar #'third fechados) :test #'equal))  ; Verifica se está em fechados
              (setq abertos (append abertos (list s))))))))))

(defun dfs (tabuleiro max-level)
  "Pesquisa em profundidade"
  (let ((abertos (list (list nil nil tabuleiro 0)))  ; Inicializa a lista de abertos (stack)
        (fechados '()))                           ; Inicializa a lista de fechados
    (loop
       (if (null abertos)  ; Se a lista de abertos estiver vazia, termine a busca
           (return nil))   ; Não encontrou o objetivo
      (let ((node (first abertos)))  ; Pega o primeiro nó da lista de abertos
        (setq abertos (rest abertos))  ; Remove o nó atual da lista de abertos
        (push node fechados)  ; Adiciona o nó atual à lista de fechados
        (if (tabuleiro-vaziop (third node))  ; Verifica se é o objetivo
            (return (caminho node fechados)))  ; Retorna o caminho completo até o objetivo
        (when (< (fourth node) max-level)  ; Verifica se a profundidade atual é menor que max-level
          (let ((sucessores (gerar-filhos node)))  ; Gera sucessores
            (dolist (s sucessores)  ; Itera sobre cada sucessor
              (unless (or (member (third s) (mapcar #'third abertos) :test #'equal)  ; Verifica se está em abertos
                          (member (third s) (mapcar #'third fechados) :test #'equal))  ; Verifica se está em fechados
                (setq abertos (cons s abertos)))))))))) ; Adiciona ao início da lista


(defun caminho (node fechados &optional (solucao '()))
  (let* ((node-pai (first node))  ; Pega o tabuleiro-pai do nó completo
         (operacao (second node)))  ; Pega a operação do nó completo
    (if (null node-pai)  ; Se o tabuleiro pai é nil, retornamos o caminho
        solucao   ; O caminho deve ser invertido, pois estamos acumulando as operações em ordem reversa
        (caminho node-pai  ; Chama recursivamente com o nó pai
                 fechados
                 (cons operacao solucao)))))  ; Adiciona a operação ao caminho

;; Geração de sucessores
(defun gerar-filhos (node &optional (linha 0) (coluna 0) (resultados '()))
  "Gera sucessores de forma recursiva a partir de uma matriz, ignorando células não distribuíveis."
  (let ((matriz (third node)))
    (if (>= linha (length matriz)) ; Se percorremos todas as linhas, terminar.
        resultados
      (let ((nova-linha (if (>= coluna (length (nth linha matriz)))
                            (1+ linha)
                          linha))
            (nova-coluna (if (>= coluna (length (nth linha matriz)))
                             0
                           (1+ coluna))))
        (if (and (< linha (length matriz)) ; Validar que ainda estamos na matriz.
                 (< coluna (length (nth linha matriz)))
                 (celula-distribuivelp linha coluna matriz))
            (let* ((tabuleiro-pai node)
                   (operacao (list linha coluna))
                   (filho (operador linha coluna matriz))
                   (custo (+ (fourth node) 1))
                   (resultado (list tabuleiro-pai operacao filho custo)))
              (gerar-filhos node nova-linha nova-coluna (cons resultado resultados)))
          (gerar-filhos node nova-linha nova-coluna resultados)))))) ; Avançar para a próxima célula.

(defun celula-distribuivelp (linha coluna matriz)
  "Retorna T se a célula tem peças suficientes para uma distribuição válida."
  (let ((valor (celula linha coluna matriz)))
    (and (> valor 0) t)))


