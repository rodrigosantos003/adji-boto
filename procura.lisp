;;;; puzzle.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e João Fernandes

(defvar abertos '())  ; Define a lista global abertos como vazia inicialmente

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


(defun adicionar-a-abertos (elemento)
  "Adiciona o elemento à lista global abertos se ele ainda não estiver presente."
  (unless (member elemento abertos)  ; Verifica se o elemento já está na lista
    (setq abertos (cons elemento abertos))))  ; Adiciona o elemento à lista global

(defun percorrer-linha (matriz linha coluna)
  "Percorre recursivamente uma linha da matriz e adiciona os resultados únicos à lista abertos."
  (if (< coluna (length (nth linha matriz)))  ; Verifica se ainda há colunas na linha
      (let ((resultado (list (list linha coluna) (operador linha coluna matriz))))  ; Chama a função operador e obtém o resultado
        (adicionar-a-abertos resultado)  ; Adiciona o resultado à lista global abertos
        (percorrer-linha matriz linha (1+ coluna)))  ; Continua para a próxima coluna
      nil))  ; Retorna nil ao final da linha

(defun percorrer-matriz (matriz &optional (linha 0))
  "Percorre recursivamente a matriz e adiciona os resultados únicos à lista abertos."
  (if (< linha (length matriz))  ; Verifica se ainda há linhas na matriz
      (progn
        (percorrer-linha matriz linha 0)  ; Percorre a linha atual
        (percorrer-matriz matriz (1+ linha)))  ; Chama recursivamente para a próxima linha
      abertos))  ; Retorna a lista global abertos ao final