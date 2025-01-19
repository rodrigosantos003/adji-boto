;;;; puzzle.lisp
;;;; Seletores, Funções Auxiliares e Operadores
;;;; Autores: Rodrigo Santos e João Fernandes


;;; Tabuleiros

(defun tabuleiro-vazio ()
  "Retorna um tabuleiro 2x6 (default) com as casas vazias"
  '(((0 0 0 0 0 0)(0 0 0 0 0 0)) 0 0)
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '(((4 4 4 4 4 4)(4 4 4 4 4 4)) 0 0)
)

(defun tabuleiro-captura ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '(((1 1 1 1 1 1)(2 2 2 2 2 2)) 0 0)
)


(defun tabuleiro-inicial()
 '(((8 8 8 8 8 8)(8 8 8 8 8 8)) 0 0)
)

;;; Seletores

(defun linha (l tabuleiro)
  (cond ((or (not (integerp l)) (< l 0) (> l 1))
         (error "A linha só pode ser 0 ou 1, mas recebeu ~A" l))
        (t (nth l tabuleiro)))
)


(defun celula (l c tabuleiro)
  (cond ((or (< c 0) (> c 5)) (error "A coluna tem que estar entre 0 e 5"))
        (t (nth c (linha l tabuleiro))))
)

(defun estado (node)
  (first node))

(defun pontuacao-1 (node)
  (second node))

(defun pontuacao-2 (node)
  (third node))

;;; Funções auxiliares

(defun tabuleiro-vaziop (tabuleiro)
  "Verifica se o tabuleiro está vazio"
  (every (lambda (linha) (every #'zerop linha)) tabuleiro)
)


(defun substituir-posicao (idx lista &optional (valor 0))
  (cond ((zerop idx) (cons valor (cdr lista)))
    (t (cons (car lista)
             (substituir-posicao (1- idx) (cdr lista) valor)))))

(defun substituir (l c tabuleiro &optional (valor 0))
  "Substitui o valor na posição (l, c) do tabuleiro pelo 'valor'."
  (cond
    ((zerop l) (cons (substituir-posicao c (car tabuleiro) valor) (cdr tabuleiro)))
    (t (cons (car tabuleiro) (substituir (1- l) c (cdr tabuleiro) valor))))
)

(defun incrementar-posicao (l c tabuleiro)
  (substituir l c tabuleiro (1+ (celula l c tabuleiro)))
)

;;; Operadores
(defun distribuir-pecas (num-pecas linha col &optional (linha-inicial linha) (col-inicial col))
  "Retorna uma lista com os pares de índices onde serão colocadas as peças, no sentido anti-horário."
  (let* ((num-linhas (length (estado (tabuleiro-inicial))))
         (num-colunas (length (first (estado (tabuleiro-inicial))))))
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

         (cond ((and (= proxima-linha linha-inicial) (= proxima-col col-inicial)) (distribuir-pecas num-pecas proxima-linha proxima-col linha-inicial col-inicial))
               (t (cons (list proxima-linha proxima-col)
               (distribuir-pecas (1- num-pecas) proxima-linha proxima-col linha-inicial col-inicial)))))))))


(defun aplicar-operador (tabuleiro linha-idx col-idx)
  "Aplica o operador às peças do buraco especificado."
  (let* ((pecas-a-retirar (celula linha-idx col-idx tabuleiro))
         (tabuleiro-sem-pecas (substituir linha-idx col-idx tabuleiro 0))
         (posicoes-distribuicao (distribuir-pecas pecas-a-retirar linha-idx col-idx))
         (ultima-posicao (car (last posicoes-distribuicao)))
         (ultima-linha (first ultima-posicao))
         (ultima-coluna (second ultima-posicao))
         (novo-tabuleiro (incrementar-posicoes posicoes-distribuicao tabuleiro-sem-pecas))
         (pecas-na-ultima (celula ultima-linha ultima-coluna novo-tabuleiro)))
    ;; Verifica se as peças na última posição são 1, 3 ou 5 para retirar as peças, senão retorna o tabuleiro com as peças distribuídas
    (let ((tabuleiro-final
           (if (and (/= ultima-linha linha-idx) (member pecas-na-ultima '(1 3 5)))  ; Corrigido: removidos os parênteses excessivos
               (substituir ultima-linha ultima-coluna novo-tabuleiro 0)  ; Zera a última célula
               novo-tabuleiro)))  ; Caso contrário, retorna o tabuleiro com as peças distribuídas normalmente
      tabuleiro-final)))


(defun incrementar-posicoes (posicoes tabuleiro)
  (if (null posicoes) ; Caso base: lista de posições vazia
      tabuleiro
      (let ((l (first (first posicoes)))  ; Linha
            (c (second (first posicoes)))) ; Coluna
        (incrementar-posicoes (rest posicoes) ; Chamada recursiva com a cauda da lista
                               (incrementar-posicao l c tabuleiro))))) ; Incrementa a célula

(defun gerar-operadores (tabuleiro linha &optional (coluna 0) (resultados '()))
  (if (>= coluna (length (first tabuleiro))) ; Verifica se todas as linhas foram percorridas.
      (reverse resultados)                   ; Retorna os resultados em ordem.
      (let ((nova-coluna (if (>= coluna (length (nth linha tabuleiro)))
                             0
                             (1+ coluna)))) ; Corrigido parêntese
        (if (and (< linha (length tabuleiro)) ; Verifica se ainda está dentro da matriz.
                 (< coluna (length (nth linha tabuleiro)))
                 (celula-distribuivelp linha coluna tabuleiro)) ; Condição customizável.
            (gerar-operadores tabuleiro linha nova-coluna
                               (cons (lambda (tabuleiro) (aplicar-operador tabuleiro linha coluna))
                                     resultados)) ; Adiciona o lambda à lista.
            (gerar-operadores tabuleiro linha nova-coluna resultados)))))

(defun celula-distribuivelp (linha coluna matriz)
  "Retorna T se a célula tem peças suficientes para uma distribuição válida."
  (let ((valor (celula linha coluna matriz)))
    (and (> valor 0) t)))

(defun soma-pecas (tabuleiro)
 (+ (apply '+ (first tabuleiro)) (apply '+ (second tabuleiro)))
)

(defun heuristicaBase (no)
  (let* ((noRaiz (raiz no))
         (pecas-no (soma-pecas (estado no)))
         (pecas-raiz (soma-pecas (estado noRaiz)))
         (pecas-capturadas (- pecas-raiz pecas-no)))
      (- pecas-no pecas-capturadas)))

(defun heuristicaPersonalizada (no)
  (let* ((pecas-no (soma-pecas (estado no)))
         (media-pecas-eliminadas (verificar-pecas-eliminadas (estado no))))
      (- pecas-no media-pecas-eliminadas)))

(defun pecas-eliminadas (linha coluna tabuleiro)
  (let* ((ultima-posicao (first (last (distribuir-pecas (celula linha coluna tabuleiro) linha coluna tabuleiro))))
         (ultima-posicao-pecas (celula (first ultima-posicao) (second ultima-posicao) tabuleiro))
         (num-pecas (+ (ceiling (celula linha coluna tabuleiro) 12) ultima-posicao-pecas)))
    (cond
      ((= num-pecas 1) 1)
      ((= num-pecas 3) 3)
      ((= num-pecas 5) 5)
      (t 0))))


(defun verificar-pecas-eliminadas (tabuleiro &optional (linha 0) (coluna 0) (resultados '()))
  (if (>= linha (length tabuleiro)) ; Verifica se todas as linhas foram percorridas.
      (let* ((resultados-sem-zero (remove-if-not #'(lambda (x) (> x 0)) resultados)) ; Filtra os 0.
             (total (apply '+ resultados-sem-zero)) ; Soma dos resultados não nulos.
             (quantidade (length resultados-sem-zero))) ; Quantidade de elementos não nulos.
        (if (> quantidade 0) ; Verifica se há elementos para calcular a média.
            (floor (/ total quantidade)) ; Calcula a média.
            0)) ; Se não houver elementos não nulos, retorna 0.
    (let ((nova-linha (if (>= coluna (length (nth linha tabuleiro)))
                          (1+ linha)
                        linha))
          (nova-coluna (if (>= coluna (length (nth linha tabuleiro)))
                           0
                         (1+ coluna))))
      (if (and (< linha (length tabuleiro)) ; Verifica se ainda está dentro da matriz.
               (< coluna (length (nth linha tabuleiro)))
               (celula-distribuivelp linha coluna tabuleiro)) ; Condição customizável.
          (verificar-pecas-eliminadas tabuleiro nova-linha nova-coluna
           (cons (pecas-eliminadas linha coluna tabuleiro) resultados)) ; Adiciona o lambda à lista.
        (verificar-pecas-eliminadas tabuleiro nova-linha nova-coluna resultados)))))


