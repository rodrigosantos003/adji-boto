;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e João Fernandes

(defun bfs (tabuleiro)
  (let ((abertos (list (list nil nil tabuleiro)))  ; Inicializa a lista de abertos
        (fechados '()))                           ; Inicializa a lista de fechados
    (loop
       (if (null abertos)  ; Se a lista de abertos estiver vazia, termine a busca
           (return nil))   ; Não encontrou o objetivo
      (let ((node (first abertos)))  ; Pega o primeiro nó da lista de abertos
        (setq abertos (rest abertos))  ; Remove o nó atual da lista de abertos
        (push node fechados)  ; Adiciona o nó atual à lista de fechados
        (if (tabuleiro-vaziop (third node))  ; Verifica se é o objetivo
            (return node))  ; Retorna o nó objetivo
        (let ((sucessores (gerar-filhos (third node))))  ; Gera sucessores
          (dolist (s sucessores)  ; Itera sobre cada sucessor
            (unless (or (member (third s) (mapcar #'third abertos) :test #'equal)  ; Verifica se está em abertos
                        (member (third s) (mapcar #'third fechados) :test #'equal))  ; Verifica se está em fechados
              (push s abertos))))))))  ; Adiciona o sucessor ao início da lista abertos


(defun gerar-filhos (matriz &optional (linha 0) (coluna 0) (resultados '()))
  "Gera sucessores a partir de uma matriz, ignorando células não distribuíveis."
  (if (< linha (length matriz)) ; Verifica se a linha é válida
      (progn
        (if (< coluna (length (nth linha matriz))) ; Verifica se a coluna é válida
            (progn
              ;; Verifica se a célula pode gerar sucessores
              (if (celula-distribuivelp linha coluna matriz)
                  (let* ((tabuleiro-pai (copy-tree matriz))
                         (operacao (list linha coluna))
                         (filho (operador linha coluna tabuleiro-pai))
                         (resultado (list tabuleiro-pai operacao filho)))
                    (gerar-filhos matriz (+ coluna 1) 0 (cons resultado resultados)))
                ;; Caso não seja distribuível, avança para a próxima coluna
                (gerar-filhos matriz linha (+ coluna 1) resultados)))
          ;; Avança para a próxima linha
          (gerar-filhos matriz (+ linha 1) 0 resultados)))
    resultados)) ; Retorna os resultados


(defun celula-distribuivelp (linha coluna matriz)
  "Retorna T se a célula tem peças suficientes para uma distribuição válida."
  (let ((valor (celula linha coluna matriz)))
    (and (> valor 0)
         ;; Adicione aqui a regra para peças suficientes, se necessário
         t)))  ; Ajuste ou remova este `t` dependendo da regra específica


