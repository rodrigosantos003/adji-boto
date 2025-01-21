;;; Ficheiro: algoritmo.lisp
;;; Algoritmo AlfaBeta
;;; Autores: João Fernandes e Rodrigo Santos

(defparameter *numero-nos-analisados* 0.0)
(defparameter *numero-cortes* 0.0)

(defun negamax (node depth jogador &optional (alpha most-negative-fixnum) (beta most-positive-fixnum) (color 1))
  "Implementa o algoritmo NegaMax com cortes Alfa-Beta. Trata nós sem sucessores válidos gerando um 'sucessor' igual ao estado atual."
  (if (or (zerop depth) (terminalp node))
      (* color (evaluate node jogador)) ; Retorna o valor avaliado do nó terminal
    (let ((children (or (sucessores node jogador) (list node)))) ; Se não houver sucessores, usa o estado atual
      (reduce
       (lambda (best-score child)
         (incrementar-nos) ; Incrementa o contador para cada sucessor processado
         (let ((score (- (negamax child (1- depth) (alternar-jogador jogador) (- beta) (- alpha) (- color)))))
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

(defun evaluate (node jogador)
  (let ((pontuacao (- (pontuacao-1 node) (pontuacao-2 node))))
    (if (= jogador 1) 
        pontuacao
        (- pontuacao))))


(defun sucessores (node jogador)
  (mapcar (lambda (op) (gerar-node node (funcall op (estado node)) jogador)) 
          (gerar-operadores (estado node) (linha-jogador jogador))))

(defun gerar-node (nodeAntigo estadoNovo jogador)
  (let ((pecas-capturadas (- (soma-pecas (estado nodeAntigo)) (soma-pecas estadoNovo))))
    (if (= jogador 1)
        (list estadoNovo (+ (pontuacao-1 nodeAntigo) pecas-capturadas) (pontuacao-2 nodeAntigo))
        (list estadoNovo (pontuacao-1 nodeAntigo) (+ (pontuacao-2 nodeAntigo) pecas-capturadas)))))

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