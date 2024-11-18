;;;; puzzle.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e Jo�o Fernandes

;;; BFS
(defun bfs (abertos fechados)
  "Algoritmo BFS que retorna o caminho para o n� objetivo."
  (cond
    ((null abertos) nil) ; Falha se n�o h� mais n�s a explorar
    (t
     (let* ((no-atual (pop abertos))         ; Remove o primeiro n� da fila
            (estado (car no-atual))          ; Estado atual
            (caminho (cdr no-atual)))        ; Caminho percorrido at� o estado
       (if (tabuleiro-vaziop estado)        ; Se o estado atual � a solu��o
           (reverse (cons estado caminho))  ; Retorna o caminho completo
           (let ((sucessores (gerar-sucessores estado)))
             ;; Adiciona novos sucessores � fila
             (dolist (s sucessores)
               (unless (or (member s fechados :test #'equal)
                           (member s (mapcar #'car abertos) :test #'equal))
                 (push (cons s (cons estado caminho)) abertos)))
             ;; Adiciona o estado atual aos fechados
             (bfs abertos (cons estado fechados)))))))
)

(defun percorrer-linha (matriz linha coluna resultados)
  "Percorre recursivamente uma linha da matriz e acumula os resultados da fun��o operador."
  (if (< coluna (length (nth linha matriz)))  ; Verifica se ainda h� colunas na linha
      (let ((resultado (list (list linha coluna) (operador linha coluna matriz))))  ; Chama a fun��o operador e obt�m o resultado
        (percorrer-linha matriz linha (1+ coluna) (cons resultado resultados)))  ; Acumula o resultado na lista
      resultados))  ; Retorna a lista acumulada quando n�o h� mais colunas

(defun percorrer-matriz (matriz &optional (linha 0) (resultados '()))
  "Percorre recursivamente a matriz e acumula os resultados da fun��o operador."
  (if (< linha (length matriz))  ; Verifica se ainda h� linhas na matriz
      (let ((novos-resultados (percorrer-linha matriz linha 0 resultados)))  ; Percorre a linha e acumula os resultados
        (percorrer-matriz matriz (1+ linha) novos-resultados))  ; Chama recursivamente para a pr�xima linha
      resultados))  ; Retorna a lista com todos os resultados ao final

