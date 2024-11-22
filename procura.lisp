;;;; puzzle.lisp
;;;; Alogritmos de porcura
;;;; Autores: Rodrigo Santos e Jo�o Fernandes


;;; ((1 2 3 4 5 6) (1 2 3 4 5 6))


;(if (or (member novo-resultado resultados :test #'equal)  ; J� est� nos resultados locais
 ;               (member novo-resultado abertos :test #'equal)     ; Est� na lista global abertos
  ;              (member novo-resultado fechados :test #'equal))   ; Est� na lista global fechados

(defun bfs (tabuleiro)
  (let ((abertos (list (list nil nil tabuleiro)))  ; Inicializa a lista de abertos
        (fechados '()))                           ; Inicializa a lista de fechados
    (loop
      (if (null abertos)  ; Se a lista de abertos estiver vazia, termine a busca
          (return nil))   ; N�o encontrou o objetivo
      (let ((node (first abertos)))  ; Pega o primeiro n� da lista de abertos
        (setq abertos (rest abertos))  ; Remove o n� atual da lista de abertos
        (push node fechados)  ; Adiciona o n� atual � lista de fechados
        (let ((sucessores (gerar-filhos (third node) abertos fechados)))  ; Gera sucessores
          (loop for s in sucessores
                when (tabuleiro-vaziop (third s))  ; Verifica se � o objetivo
                  do (return s)  ; Retorna o n� objetivo
                unless (or (member (third s) (mapcar #'third abertos) :test #'equal)  ; Verifica se est� em abertos
                           (member (third s) (mapcar #'third fechados) :test #'equal))  ; Verifica se est� em fechados
                  do (push s abertos)))))))

(defun gerar-filhos (matriz abertos fechados &optional (linha 0) (coluna 0) (resultados '()))
  (format t "Debug: linha = ~A, coluna = ~A, matriz = ~A ~%" linha coluna matriz)
  (format t "Debug: tamanho nth = ~A ~%" (length (nth linha matriz)))
  (if (< linha (length matriz)) ; Verifica se a linha � v�lida
      (progn
        (format t "Debug (linha v�lida): linha = ~A, matriz-length = ~A ~%" linha (length matriz))
        (if (< coluna (length (nth linha matriz))) ; Verifica se a coluna � v�lida
            (progn
              (format t "Debug (coluna v�lida): linha = ~A, coluna = ~A, matriz-length = ~A ~%" linha coluna (length (nth linha matriz)))
              (let* ((tabuleiro-pai (copy-tree matriz))
                     (operacao (list linha coluna))
                     (filho (operador linha coluna tabuleiro-pai))
                     (resultado (list tabuleiro-pai operacao filho)))
                (gerar-filhos matriz abertos fechados linha (+ coluna 1) 
                              (cons resultado resultados)))) ; Adiciona `resultado` � lista
          (progn
            (format t "Debug (coluna inv�lida): linha = ~A, coluna = ~A ~%" linha coluna)
            (gerar-filhos matriz abertos fechados (+ linha 1) 0 resultados)))) ; Avan�a para a pr�xima linha
    (progn
      (format t "Debug (linha inv�lida): linha = ~A, matriz-length = ~A ~%" linha (length matriz))
      resultados))) ; Retorna os resultados
