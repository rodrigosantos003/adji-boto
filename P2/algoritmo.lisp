;;; Ficheiro: algoritmo.lisp
;;; Algoritmo AlfaBeta
;;; Autores: Jo�o Fernandes e Rodrigo Santos

(defparameter *numero-nos-analisados* 0.0)
(defparameter *numero-cortes* 0.0)


(defun negamax (node depth jogador &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (color 1))
  (if (or (zerop depth) (terminalp node))
      (* color (evaluate node jogador))
      (negamax-recursivo node depth jogador alpha beta color (or (remove nil (sucessores node jogador)) (list node)))))

(defun negamax-recursivo (node depth jogador alpha beta color children)
  (if (null children)
      alpha
      (let* ((child (car children))
             (score (- (negamax child (1- depth) (alternar-jogador jogador) (- beta) (- alpha) (- color)))))
        (incrementar-nos)
        (let ((new-alpha (max alpha score)))
          (if (>= new-alpha beta)
              (progn
                (incrementar-cortes)
                beta)
              (negamax-recursivo node depth jogador new-alpha beta color (cdr children)))))))



(defun terminalp (node)
  (tabuleiro-vaziop (estado node)))

(defun evaluate (node jogador)
  (let ((pontuacao (- (pontuacao-1 node) (pontuacao-2 node))))
    (if (= jogador 1) 
        pontuacao
        (- pontuacao))))

(defun sucessores (node jogador)
(mapcar (lambda (op)
              (when op
                (gerar-node node (funcall op (estado node)) jogador)))
            (gerar-operadores (estado node) (linha-jogador jogador))))


(defun gerar-node (nodeAntigo estadoNovo jogador)
  (let ((pecas-capturadas (- (soma-pecas (estado nodeAntigo)) (soma-pecas estadoNovo))))
    (if (= jogador 1)
        (list estadoNovo (list (+ (pontuacao-1 nodeAntigo) pecas-capturadas) (pontuacao-2 nodeAntigo)))
        (list estadoNovo (list (pontuacao-1 nodeAntigo) (+ (pontuacao-2 nodeAntigo) pecas-capturadas))))))

(defun incrementar-nos ()
  "Incrementa o contador de n�s analisados"
  (incf *numero-nos-analisados*))

(defun incrementar-cortes ()
  "Incrementa o contador de cortes"
  (incf *numero-cortes*))

(defun repor-contagem ()
  "Rep�e a contagem de n�s analisados e de cortes"
  (setf *numero-nos-analisados* 0.0)
  (setf *numero-cortes* 0.0)
)