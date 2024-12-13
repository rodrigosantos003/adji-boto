;;; Ficheiro: desempenho.lisp
;;; Fun��es para calcular as medidas de desempenho dos algoritmos
;;; Autores: Jo�o Fernandes e Rodrigo Santos

;; Vari�vel global para contar o n�mero de n�s gerados
(defparameter *numero-nos-gerados* 0.0)

;; Fun��o para incrementar o contador de n�s gerados
(defun incrementar-nos ()
  (incf *numero-nos-gerados*))

(defun resetar-contar-nos ()
  "Reseta o contador de n�s gerados para 0."
  (setf *numero-nos-gerados* 0.0))


;; Penetr�ncia
(defun penetrancia (caminho)
  "Calcula a penetr�ncia de um algortimo (P = L/T)"
  (/ (comprimento-caminho caminho) *numero-nos-gerados*)
)

(defun comprimento-caminho (c)
  (length c)
)

(defun total-nos (abertos fechados)
  (+ (length abertos) (length fechados))
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