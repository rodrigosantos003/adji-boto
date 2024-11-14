;;;; puzzle.lisp
;;;; Seletores, Funções Auxiliares e Operadores
;;;; Autores: Rodrigo Santos e João Fernandes


;;; Tabuleiros

(defun tabuleiro-vazio (&optional (linhas 2) (colunas 6))
  "Retorna um tabuleiro 2x6 (default) com as casas vazias"
  (make-list linhas :initial-element (make-list colunas :initial-element '0))
)

(defun tabuleiro-teste ()
  "Retorna um tabuleiro de teste 2x6 que corresponde ao tabuleiro d) do enunciado do projeto"
  '((1 2 3 4 5 6)
    (6 5 4 3 2 1))
)

;;; Seletores

(defun linha (l tabuleiro)
  "Retorna uma lista que representa uma linha do tabuleiro"
  (cond ((and (/= l 0) (/= l 1)) (error "A linha só pode ser 0 ou 1"))
        (t (nth l tabuleiro)))
)

(defun celula (l c tabuleiro)
  (cond ((or (< c 0) (> c 5)) (error "A coluna tem que estar entre 0 e 5"))
        (t (nth c (linha l tabuleiro))))
)

;;; Funções auxiliares

(defun tabuleiro-vaziop (tabuleiro)
  "Verifica se o tabuleiro está vazio"
  (cond
    ((null tabuleiro) t)
    ((null (car tabuleiro)) (tabuleiro-vaziop (cdr tabuleiro)))
    ((/= (car (car tabuleiro)) 0) nil)
    (t (tabuleiro-vaziop (cons (cdr (car tabuleiro)) (cdr tabuleiro)))))
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
(defun distribuir-pecas (num-pecas linha col &optional (tabuleiro (tabuleiro-vazio)))
  "Retorna uma lista com os pares de índices onde serão colocadas as peças."
  (cond
    ((<= num-pecas 0) '())

    (t
     (let* ((proxima-col (mod (+ col 1) 6))
            (proxima-linha (if (= proxima-col 0)
                               (if (= linha 0) 1 0)
                               linha))
            
            (nova-linha (if (and (= proxima-linha linha) (= proxima-col col))
                            (if (= proxima-linha 0) 1 0)
                            proxima-linha))
            (nova-coluna (if (and (= proxima-linha linha) (= proxima-col col))
                             (+ proxima-col 1)
                             proxima-col)))
       
       (cons (list nova-linha nova-coluna)
             (distribuir-pecas (1- num-pecas) nova-linha nova-coluna))
      )
    )
  )
)

(defun operador (linha-idx col-idx tabuleiro)
  "Recebe dois índices e o tabuleiro, retira as peças das posições passadas e distribui novas peças, com a condição de remoção se necessário."
  (let* ((pecas-a-retirar (celula linha-idx col-idx tabuleiro))
         (tabuleiro-sem-pecas (substituir linha-idx col-idx tabuleiro 0)) ;; Retira as peças da posição indicada
         (posicoes-distribuicao (distribuir-pecas pecas-a-retirar linha-idx col-idx tabuleiro-sem-pecas)) 
         (ultima-posicao (car (last posicoes-distribuicao))) ;; Última posição após distribuição
         (ultima-linha (first ultima-posicao))
         (ultima-coluna (second ultima-posicao))
         (pecas-na-ultima (celula ultima-linha ultima-coluna tabuleiro)))
    
    ;; Verificar se a última posição contém 1, 3 ou 5 peças e se sim, retira-as
    (if (member pecas-na-ultima '(1 3 5))
        (substituir ultima-linha ultima-coluna tabuleiro 0)
        tabuleiro
    )
  )
)
