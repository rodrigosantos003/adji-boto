;;; Ficheiro: algoritmo.lisp
;;; Algoritmo AlfaBeta
;;; Autores: João Fernandes e Rodrigo Santos

(defvar *negamax-memo* (make-hash-table :test 'equal))

(defun negamax (node depth alpha beta color)
  "Implementa o algoritmo NegaMax com cortes Alfa-Beta com memoização usando mapcar para processar filhos."
  (let ((memo-key (list node depth color)))
    ;; Verifica se o valor já foi memoizado
    (or (gethash memo-key *negamax-memo*)
        (setf (gethash memo-key *negamax-memo*)
              (if (or (zerop depth) (terminalp node))
                  (* color (evaluate node)) ; Retorna o valor avaliado do nó terminal
                (let ((children (sucessores node color)))
                  (reduce
                   (lambda (best-score child)
                     (let ((score (- (negamax child (1- depth) (- beta) (- alpha) (- color)))))
                       (let ((new-alpha (max alpha score)))
                         (if (>= new-alpha beta)
                             (return-from negamax new-alpha) ; Corta beta
                           (max best-score score)))))
                   children
                   :initial-value most-negative-fixnum)))))))

(defun clear-negamax-memo ()
  "Limpa a tabela de memoização."
  (clrhash *negamax-memo*))

(defun terminalp (node)
  (tabuleiro-vaziop (estado node))
)

(defun evaluate (node)
    (- (pontuacao-1 node) (pontuacao-2 node)))

(defun sucessores (node jogador)
  (mapcar (lambda (op) (gerar-node node (funcall op (estado node)) jogador)) 
          (gerar-operadores (estado node) (if (= jogador -1) 0 1))))

(defun gerar-node (nodeAntigo estadoNovo color)
  (let ((pecas-capturadas (- (soma-pecas (estado nodeAntigo)) (soma-pecas estadoNovo))))
    (if (= color -1)
        (list estadoNovo (pontuacao-1 nodeAntigo) (+ (pontuacao-2 nodeAntigo) pecas-capturadas))
        (list estadoNovo (+ (pontuacao-1 nodeAntigo) pecas-capturadas) (pontuacao-2 nodeAntigo)))))

