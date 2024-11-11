;;;; laboratorio7.lisp
;;;; Ficha de LaboratÃ³rio nÂº7 - Apoio ao 1Âº Projeto
;;;; Autor: 


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


;;; Exercicios

(defun linha (l tabuleiro)
  "Retorna uma lista que representa uma linha do tabuleiro"
  (cond ((and (/= l 0) (/= l 1)) (error "A linha só pode ser 0 ou 1"))
        (t (nth l tabuleiro)))
)

(defun celula (l c tabuleiro)
  (cond ((or (< c 0) (> c 5)) (error "A coluna tem que estar entre 0 e 5"))
        (t (nth c (linha l tabuleiro))))
)

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