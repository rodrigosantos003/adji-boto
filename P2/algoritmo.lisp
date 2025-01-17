;;; Ficheiro: algoritmo.lisp
;;; Algoritmo AlfaBeta
;;; Autores: João Fernandes e Rodrigo Santos

(defparameter *numero-nos-analisados* 0.0)
(defparameter *numero-cortes* 0.0)

(defun negamax (node depth &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (color 1))
  "Implementa o algoritmo NegaMax com cortes Alfa-Beta usando mapcar para processar filhos."
  (if (or (zerop depth) (terminalp node))
      (* color (evaluate node)) ; Retorna o valor avaliado do nó terminal
    (let ((children (sucessores node color)))
      (reduce
       (lambda (best-score child)
         (incrementar-nos) ; Incrementa o contador para cada sucessor individual
         (let ((score (- (negamax child (1- depth) (- beta) (- alpha) (- color)))))
           (let ((new-alpha (max alpha score)))
             (if (>= new-alpha beta)
                 (progn
                   (incrementar-cortes) ; Incrementa o contador de cortes
                   (return-from negamax new-alpha)) ; Corta beta
               (max best-score score)))))
       children
       :initial-value most-negative-fixnum))))

(defun terminalp (node)
  (tabuleiro-vaziop (estado node)))

(defun evaluate (node)
  (- (pontuacao-2 node) (pontuacao-1 node)))

(defun sucessores (node cor)
  (mapcar (lambda (op) (gerar-node node (funcall op (estado node)) cor)) 
          (gerar-operadores (estado node) (linha-jogador cor))))

(defun gerar-node (nodeAntigo estadoNovo color)
  (let ((pecas-capturadas (- (soma-pecas (estado nodeAntigo)) (soma-pecas estadoNovo))))
    (if (= (linha-jogador color) 1)
        (list estadoNovo (pontuacao-1 nodeAntigo) (+ (pontuacao-2 nodeAntigo) pecas-capturadas))
        (list estadoNovo (+ (pontuacao-1 nodeAntigo) pecas-capturadas) (pontuacao-2 nodeAntigo)))))

(defun linha-jogador (color)
  (if (= color -1) 0 1))

(defun incrementar-nos ()
  "Incrementa o contador de nós analisados"
  (incf *numero-nos-analisados*))

(defun incrementar-cortes ()
  "Incrementa o contador de cortes"
  (incf *numero-cortes*))

(defun repor-contagem ()
  "Repõe a contagem de nós analisados e de cortes"
  (setf *numero-nos-analisados* 0.0)
  (setf *numero-cortes* 0.0)
)