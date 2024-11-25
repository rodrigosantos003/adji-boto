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
                   (resultado (list tabuleiro-pai operacao filho)))
              (gerar-filhos node nova-linha nova-coluna (cons resultado resultados)))
          (gerar-filhos node nova-linha nova-coluna resultados)))))) ; Avançar para a próxima célula.




(defun celula-distribuivelp (linha coluna matriz)
  "Retorna T se a célula tem peças suficientes para uma distribuição válida."
  (let ((valor (celula linha coluna matriz)))
    (and (> valor 0) t)))


