;;;; puzzle.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e Jo�o Fernandes

(defvar abertos '())  ; Define a lista global abertos como vazia inicialmente

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


(defun adicionar-a-abertos (elemento)
  "Adiciona o elemento � lista global abertos se ele ainda n�o estiver presente."
  (unless (member elemento abertos)  ; Verifica se o elemento j� est� na lista
    (setq abertos (cons elemento abertos))))  ; Adiciona o elemento � lista global

(defun percorrer-linha (matriz linha coluna)
  "Percorre recursivamente uma linha da matriz e adiciona os resultados �nicos � lista abertos."
  (if (< coluna (length (nth linha matriz)))  ; Verifica se ainda h� colunas na linha
      (let ((resultado (list (list linha coluna) (operador linha coluna matriz))))  ; Chama a fun��o operador e obt�m o resultado
        (adicionar-a-abertos resultado)  ; Adiciona o resultado � lista global abertos
        (percorrer-linha matriz linha (1+ coluna)))  ; Continua para a pr�xima coluna
      nil))  ; Retorna nil ao final da linha

(defun percorrer-matriz (matriz &optional (linha 0))
  "Percorre recursivamente a matriz e adiciona os resultados �nicos � lista abertos."
  (if (< linha (length matriz))  ; Verifica se ainda h� linhas na matriz
      (progn
        (percorrer-linha matriz linha 0)  ; Percorre a linha atual
        (percorrer-matriz matriz (1+ linha)))  ; Chama recursivamente para a pr�xima linha
      abertos))  ; Retorna a lista global abertos ao final