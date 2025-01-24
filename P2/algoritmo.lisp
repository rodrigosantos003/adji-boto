;;; Ficheiro: algoritmo.lisp
;;; Algoritmo AlfaBeta
;;; Autores: João Fernandes e Rodrigo Santos

(defparameter *numero-nos-analisados* 0.0)
(defparameter *numero-cortes* 0.0)

(defvar *negamax-cache* (make-hash-table :test 'equal))

(defun negamax (node depth jogador &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (color 1))
  "Implementa o algoritmo Negamax com memorização e poda alfa-beta."
  (let ((cache-key (list node depth jogador alpha beta color)))
    (or (gethash cache-key *negamax-cache*)
        (setf (gethash cache-key *negamax-cache*)
              (if (or (zerop depth) (terminalp node))
                  (* color (evaluate node jogador))
                (labels ((negamax-recursivo (children alpha beta)
                           (if (null children)
                               alpha
                             (progn
                               (incrementar-nos)
                               (let* ((new-alpha (max alpha (- (negamax (car children)
                                                                 (1- depth)
                                                                 (alternar-jogador jogador)
                                                                 (- beta)
                                                                 (- alpha)
                                                                 (- color))))))
                                 (if (>= new-alpha beta)
                                     (progn
                                       (incrementar-cortes)
                                       beta)
                                   (negamax-recursivo (cdr children) new-alpha beta)))))))
                  (negamax-recursivo (or (remove nil (sucessores node jogador)) (list node)) alpha beta)))))))

(defun terminalp (node)
  (tabuleiro-vaziop (estado node)))

(defun evaluate (node jogador)
    (if (= jogador 1) 
        (- (pontuacao-1 node) (pontuacao-2 node))
        (- (pontuacao-2 node) (pontuacao-1 node))))

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