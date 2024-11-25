;;;; procura.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e Jo�o Fernandes

(defun bfs (tabuleiro)
  (let ((abertos (list (list nil nil tabuleiro)))  ; Inicializa a lista de abertos
        (fechados '()))                           ; Inicializa a lista de fechados
    (loop
       (if (null abertos)  ; Se a lista de abertos estiver vazia, termine a busca
           (return nil))   ; N�o encontrou o objetivo
      (let ((node (first abertos)))  ; Pega o primeiro n� da lista de abertos
        (setq abertos (rest abertos))  ; Remove o n� atual da lista de abertos
        (push node fechados)  ; Adiciona o n� atual � lista de fechados
        (if (tabuleiro-vaziop (third node))  ; Verifica se � o objetivo
            (return node))  ; Retorna o n� objetivo
        (let ((sucessores (gerar-filhos (third node))))  ; Gera sucessores
          (dolist (s sucessores)  ; Itera sobre cada sucessor
            (unless (or (member (third s) (mapcar #'third abertos) :test #'equal)  ; Verifica se est� em abertos
                        (member (third s) (mapcar #'third fechados) :test #'equal))  ; Verifica se est� em fechados
              (push s abertos))))))))  ; Adiciona o sucessor ao in�cio da lista abertos


(defun gerar-filhos (matriz &optional (linha 0) (coluna 0) (resultados '()))
  "Gera sucessores a partir de uma matriz, ignorando c�lulas n�o distribu�veis."
  (if (< linha (length matriz)) ; Verifica se a linha � v�lida
      (progn
        (if (< coluna (length (nth linha matriz))) ; Verifica se a coluna � v�lida
            (progn
              ;; Verifica se a c�lula pode gerar sucessores
              (if (celula-distribuivelp linha coluna matriz)
                  (let* ((tabuleiro-pai (copy-tree matriz))
                         (operacao (list linha coluna))
                         (filho (operador linha coluna tabuleiro-pai))
                         (resultado (list tabuleiro-pai operacao filho)))
                    (gerar-filhos matriz (+ coluna 1) 0 (cons resultado resultados)))
                ;; Caso n�o seja distribu�vel, avan�a para a pr�xima coluna
                (gerar-filhos matriz linha (+ coluna 1) resultados)))
          ;; Avan�a para a pr�xima linha
          (gerar-filhos matriz (+ linha 1) 0 resultados)))
    resultados)) ; Retorna os resultados


(defun celula-distribuivelp (linha coluna matriz)
  "Retorna T se a c�lula tem pe�as suficientes para uma distribui��o v�lida."
  (let ((valor (celula linha coluna matriz)))
    (and (> valor 0)
         ;; Adicione aqui a regra para pe�as suficientes, se necess�rio
         t)))  ; Ajuste ou remova este `t` dependendo da regra espec�fica


