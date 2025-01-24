;;; Ficheiro: algoritmo.lisp
;;; Algoritmo AlfaBeta
;;; Autores: João Fernandes e Rodrigo Santos

(defparameter *numero-nos-analisados* 0.0)
(defparameter *numero-cortes* 0.0)

(defun negamax (node depth jogador &optional (alpha -1000) (beta 1000) (color 1))
  "Implementação otimizada do algoritmo Negamax com recursão funcional, contagem de nós analisados e sem consumo excessivo de memória stack."
  (labels ((processar-filho (sucessores alpha beta)
             (if (null sucessores)
                 alpha
                 (let* ((value (- (negamax (car sucessores) (1- depth) 
                                             (alternar-jogador jogador) 
                                             (- beta) 
                                             (- alpha) 
                                             (- color)))))
                   (if (>= value beta)
                       (progn
                         (incrementar-cortes)
                         beta)
                       (processar-filho (cdr sucessores) (max alpha value) beta))))))

    ;; Incrementa a contagem de nós analisados
    (incrementar-nos)
    
    ;; Verifica se é um nó terminal ou profundidade zero
    (if (or (zerop depth) (terminalp node))
        (* color (evaluate node jogador))
        (let ((sucessores (remove nil (sucessores node jogador))))
          (processar-filho sucessores alpha beta)))))


(defun terminalp (node)
  (tabuleiro-vaziop (estado node)))

(defun evaluate (node jogador)
    (if (= jogador 1) 
        (- (pontuacao-1 node) (pontuacao-2 node))
        (- (pontuacao-2 node) (pontuacao-1 node))))

(defun sucessores (node jogador)
  (let ((operadores (gerar-operadores (estado node) (linha-jogador jogador))))
    (remove nil
            (mapcar (lambda (op)
                      (when op
                        (gerar-node node (funcall op (estado node)) jogador)))
                    operadores))))

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