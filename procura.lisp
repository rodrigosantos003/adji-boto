;;;; puzzle.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e João Fernandes

;;; BFS
(defun bfs (abertos fechados)
  "Algoritmo BFS que retorna o caminho para o nó objetivo."
  (cond
    ((null abertos) nil) ; Falha se não há mais nós a explorar
    (t
     (let* ((no-atual (pop abertos))         ; Remove o primeiro nó da fila
            (estado (car no-atual))          ; Estado atual
            (caminho (cdr no-atual)))        ; Caminho percorrido até o estado
       (if (tabuleiro-vaziop estado)        ; Se o estado atual é a solução
           (reverse (cons estado caminho))  ; Retorna o caminho completo
           (let ((sucessores (gerar-sucessores estado)))
             ;; Adiciona novos sucessores à fila
             (dolist (s sucessores)
               (unless (or (member s fechados :test #'equal)
                           (member s (mapcar #'car abertos) :test #'equal))
                 (push (cons s (cons estado caminho)) abertos)))
             ;; Adiciona o estado atual aos fechados
             (bfs abertos (cons estado fechados)))))))
)

(defun percorrer-linha (matriz linha coluna resultados)
  "Percorre recursivamente uma linha da matriz e acumula os resultados da função operador."
  (if (< coluna (length (nth linha matriz)))  ; Verifica se ainda há colunas na linha
      (let ((resultado (list (list linha coluna) (operador linha coluna matriz))))  ; Chama a função operador e obtém o resultado
        (percorrer-linha matriz linha (1+ coluna) (cons resultado resultados)))  ; Acumula o resultado na lista
      resultados))  ; Retorna a lista acumulada quando não há mais colunas

(defun percorrer-matriz (matriz &optional (linha 0) (resultados '()))
  "Percorre recursivamente a matriz e acumula os resultados da função operador."
  (if (< linha (length matriz))  ; Verifica se ainda há linhas na matriz
      (let ((novos-resultados (percorrer-linha matriz linha 0 resultados)))  ; Percorre a linha e acumula os resultados
        (percorrer-matriz matriz (1+ linha) novos-resultados))  ; Chama recursivamente para a próxima linha
      resultados))  ; Retorna a lista com todos os resultados ao final

