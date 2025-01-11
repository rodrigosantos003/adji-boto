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
              (if (or (zerop depth) (terminal-p node))
                  (* color (evaluate node)) ; Retorna o valor avaliado do nó terminal
                (let ((children (get-children node)))
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
  (tabuleiro-vaziop node)
)

(defun evaluate (node)
  (let ((pontuacao-1 (getf node :pontuacao-1)) ; Pontuação do jogador 1
        (pontuacao-2 (getf node :pontuacao-2))) ; Pontuação do jogador 2
    (- pontuacao-1 pontuacao-2)))

(defun get-children (node)
  "Retorna os filhos de um nó."
  (getf node :children))