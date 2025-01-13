;;; Ficheiro: algoritmo.lisp
;;; Algoritmo AlfaBeta
;;; Autores: Jo�o Fernandes e Rodrigo Santos

(defun negamax (node depth alpha beta color)
  "Implementa o algoritmo NegaMax com cortes Alfa-Beta usando mapcar para processar filhos."
  (if (or (zerop depth) (terminalp node))
      (* color (evaluate node)) ; Retorna o valor avaliado do n� terminal
    (let ((children (sucessores node color)))
      (reduce
       (lambda (best-score child)
         (let ((score (- (negamax child (1- depth) (- beta) (- alpha) (- color)))))
           (let ((new-alpha (max alpha score)))
             (if (>= new-alpha beta)
                 (return-from negamax new-alpha) ; Corta beta
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
