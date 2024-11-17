;;;; puzzle.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e Jo�o Fernandes

;;; BFS
(defun bfs (abertos fechados)
  "Algoritmo BFS recursivo para encontrar o caminho ao estado final."
  (cond
    ((null abertos) nil) ; Se n�o h� estados a explorar, falha
    (t
     (let* ((no-atual (car abertos))             ; Primeiro n� da lista
            (resto-abertos (cdr abertos)))       ; Restante dos estados a explorar
       (if (tabuleiro-vaziop no-atual)            ; Verifica se o estado atual � o objetivo
           no-atual                             ; Retorna o estado final
         (let ((sucessores (gerar-sucessores no-atual)))
           ;; Remove sucessores j� visitados e adiciona novos a abertos
           (bfs
            (append resto-abertos
                    (remove-if (lambda (s) (or (member s fechados :test #'equal)
                                              (member s abertos :test #'equal)))
                               sucessores))
            (cons no-atual fechados)))))))
)


(defun gerar-sucessores (estado)
  "Gera os sucessores aplicando os operadores v�lidos."
  (let ((sucessores '()))
    (dotimes (linha-idx 2)
      (dotimes (col-idx 6)
        (when (> (celula linha-idx col-idx estado) 0)
          (let ((novo-estado (operador linha-idx col-idx estado)))
            ;; S� adiciona estados realmente novos
            (unless (equal estado novo-estado)
              (push novo-estado sucessores))))))
    sucessores)
)

(defun gerar-todos-sucessores (estado)
  (let ((sucessores '()))
    (dotimes (linha 2 sucessores) ; Itera pelas linhas
      (dotimes (buraco 6 sucessores) ; Itera pelos buracos
        (when (> (nth buraco (nth linha estado)) 0) ; Verifica se h� pe�as no buraco
          (push (operador estado linha buraco) sucessores)))))
)