;;;; puzzle.lisp
;;;; Seletores, Fun��es Auxiliares e Operadores
;;;; Autores: Rodrigo Santos e Jo�o Fernandes


;;; Tabuleiros

(defun tabuleiro-vazio (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro 2x6 (default) com as casas vazias"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '((0 0 0 0 0 0)
    (0 0 0 0 1 2))
)

(defun tabuleiro-inicial()
 '((8 8 8 8 8 8)
 (8 8 8 8 8 8))
)

;;; Seletores

(defun linha (l tabuleiro)
  (cond ((or (not (integerp l)) (< l 0) (> l 1))
         (error "A linha s� pode ser 0 ou 1, mas recebeu ~A" l))
        (t (nth l tabuleiro)))
)


(defun celula (l c tabuleiro)
  (cond ((or (< c 0) (> c 5)) (error "A coluna tem que estar entre 0 e 5"))
        (t (nth c (linha l tabuleiro))))
)

;;; Fun��es auxiliares

(defun tabuleiro-vaziop (tabuleiro)
  "Verifica se o tabuleiro est� vazio"
  (every (lambda (linha) (every #'zerop linha)) tabuleiro)
)

(defun substituir-posicao (idx lista &optional (valor 0))
  (cond ((zerop idx) (cons valor (cdr lista)))
    (t (cons (car lista)
             (substituir-posicao (1- idx) (cdr lista) valor)))))

(defun substituir (l c tabuleiro &optional (valor 0))
  "Substitui o valor na posi��o (l, c) do tabuleiro pelo 'valor'."
  (cond
    ((zerop l) (cons (substituir-posicao c (car tabuleiro) valor) (cdr tabuleiro)))
    (t (cons (car tabuleiro) (substituir (1- l) c (cdr tabuleiro) valor))))
)

(defun incrementar-posicao (l c tabuleiro)
  (substituir l c tabuleiro (1+ (celula l c tabuleiro)))
)

;;; Operadores
(defun distribuir-pecas (num-pecas linha col &optional (tabuleiro (tabuleiro-vazio)))
  "Retorna uma lista com os pares de �ndices onde ser�o colocadas as pe�as, no sentido anti-hor�rio."
  (let* ((num-linhas (length tabuleiro))
         (num-colunas (length (car tabuleiro))))
    (cond
      ((<= num-pecas 0) '()) 
      (t
       (let* ((proxima-linha 
               (cond 
                ((and (= linha 0) (> col 0)) linha) ; parte superior
                ((and (= col 0) (< linha (1- num-linhas))) (1+ linha)) ; lado esquerdo
                ((and (= linha (1- num-linhas)) (< col (1- num-colunas))) linha) ; parte inferior
                ((and (= col (1- num-colunas)) (> linha 0)) (1- linha)))) ; lado direito
              (proxima-col
               (cond 
                ((and (= linha 0) (> col 0)) (1- col)) ; parte superior
                ((and (= col 0) (< linha (1- num-linhas))) col) ; lado esquerdo
                ((and (= linha (1- num-linhas)) (< col (1- num-colunas))) (1+ col)) ; parte inferior
                ((and (= col (1- num-colunas)) (> linha 0)) col)))) ; lado direito
         
         (cons (list proxima-linha proxima-col)
               (distribuir-pecas (1- num-pecas) proxima-linha proxima-col tabuleiro))))))
)


(defun aplicar-operador (tabuleiro linha-idx col-idx)
  "Aplica o operador �s pe�as do buraco especificado."
  (let* ((pecas-a-retirar (celula linha-idx col-idx tabuleiro))
         (tabuleiro-sem-pecas (substituir linha-idx col-idx tabuleiro 0))
         (posicoes-distribuicao (distribuir-pecas pecas-a-retirar linha-idx col-idx tabuleiro-sem-pecas))
         (ultima-posicao (car (last posicoes-distribuicao)))
         (ultima-linha (first ultima-posicao))
         (ultima-coluna (second ultima-posicao))
         (novo-tabuleiro (incrementar-posicoes posicoes-distribuicao tabuleiro-sem-pecas))
         (pecas-na-ultima (celula ultima-linha ultima-coluna novo-tabuleiro)))
    ;; Verifica se as pe�as na �ltima posi��o s�o 1, 3 ou 5 para retirar as pe�as, sen�o retorna o tabuleiro com as pe�as distribu�das
    (let ((tabuleiro-final
           (if (member pecas-na-ultima '(1 3 5))  ; Se a �ltima c�lula tiver 1, 3 ou 5 pe�as
               (substituir ultima-linha ultima-coluna novo-tabuleiro 0)  ; Zera a �ltima c�lula
               novo-tabuleiro)))  ; Caso contr�rio, retorna o tabuleiro com as pe�as distribu�das normalmente
      tabuleiro-final))
)

(defun incrementar-posicoes (posicoes tabuleiro)
  (if (null posicoes) ; Caso base: lista de posi��es vazia
      tabuleiro
      (let ((l (first (first posicoes)))  ; Linha
            (c (second (first posicoes)))) ; Coluna
        (incrementar-posicoes (rest posicoes) ; Chamada recursiva com a cauda da lista
                               (incrementar-posicao l c tabuleiro))))) ; Incrementa a c�lula

(defun gerar-operadores (tabuleiro &optional (linha 0) (coluna 0) (resultados '()))
  (if (>= linha (length tabuleiro)) ; Verifica se todas as linhas foram percorridas.
      (reverse resultados)          ; Retorna os resultados em ordem.
    (let ((nova-linha (if (>= coluna (length (nth linha tabuleiro)))
                          (1+ linha)
                        linha))
          (nova-coluna (if (>= coluna (length (nth linha tabuleiro)))
                           0
                         (1+ coluna))))
      (if (and (< linha (length tabuleiro)) ; Verifica se ainda est� dentro da matriz.
               (< coluna (length (nth linha tabuleiro)))
               (celula-distribuivelp linha coluna tabuleiro)) ; Condi��o customiz�vel.
          (gerar-operadores tabuleiro nova-linha nova-coluna
           (cons (lambda (tabuleiro) (aplicar-operador tabuleiro linha coluna))
                 resultados)) ; Adiciona o lambda � lista.
        (gerar-operadores tabuleiro nova-linha nova-coluna resultados)))))


(defun celula-distribuivelp (linha coluna matriz)
  "Retorna T se a c�lula tem pe�as suficientes para uma distribui��o v�lida."
  (let ((valor (celula linha coluna matriz)))
    (and (> valor 0) t)))
