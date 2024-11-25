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


(defun gerar-filhos (node &optional (linha 0) (coluna 0) (resultados '()))
  "Gera sucessores de forma recursiva a partir de uma matriz, ignorando c�lulas n�o distribu�veis."
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
          (gerar-filhos node nova-linha nova-coluna resultados)))))) ; Avan�ar para a pr�xima c�lula.




(defun celula-distribuivelp (linha coluna matriz)
  "Retorna T se a c�lula tem pe�as suficientes para uma distribui��o v�lida."
  (let ((valor (celula linha coluna matriz)))
    (and (> valor 0) t)))


