;;; Ficheiro: desempenho.lisp
;;; Fun��es para calcular as medidas de desempenho dos algoritmos
;;; Autores: Jo�o Fernandes e Rodrigo Santos

;; Penetr�ncia
(defun penetrancia (comprimento-caminho total-nos)
  "Calcula a penetr�ncia de um algortimo (P = L/T)"
  (/ comprimento-caminho total-nos)
)

;; Fator de ramifica��o
(defun fator-ramificacao (comprimento-caminho total-nos a b tolerancia)
  "Calcula o fator de ramifica��o de um algoritmo (B + B^2 + � + B^L = T)"
  (metodo-bisseccao (lambda (b) (funcao-f b comprimento-caminho total-nos)) a b tolerancia)
)

(defun soma-potencias (ramificacao comprimento-caminho)
  (if (= comprimento-caminho  1)
      ramificacao
      (+ (expt ramificacao comprimento-caminho) (soma-potencias ramificacao (- comprimento-caminho 1)))))

(defun funcao-f (ramificacao comprimento-caminho total-nos)
  (- (soma-potencias ramificacao comprimento-caminho) total-nos))

(defun metodo-bisseccao (fun a b tolerancia)
  (if (< (- b a) tolerancia)(/ (+ a b) 2)
      (let* ((c (/ (+ a b) 2))
             (fa (funcall fun a))
             (fc (funcall fun c)))
        (cond
            ((= fc 0) c)
            ((< (* fa fc) 0) (metodo-bisseccao fun c b tolerancia))
            (t (metodo-bisseccao fun a c tolerancia)))))
)